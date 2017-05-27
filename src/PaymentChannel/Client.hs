module PaymentChannel.Client
( channelWithInitialPayment
, createPayment
, createClosingPayment
, getRefundBitcoinTx
)
where

import PaymentChannel.Server                            (getSettlementBitcoinTx)
import PaymentChannel.Internal.Payment
import PaymentChannel.Internal.Receiver.Util
import Bitcoin.Util                                     (calcTxSize)
import PaymentChannel.Internal.Refund                   (mkRefundTx)
import PaymentChannel.Types
import Data.Functor.Identity                            (Identity(..))
import qualified RBPCP.Types                            as RBPCP
import qualified  Network.Haskoin.Crypto                as HC
import qualified  Network.Haskoin.Transaction           as HT


data ChannelCreationError
  = FundingError CreationError          -- ^ Invalid funding output or dusty funding amount
  | InsufficientFundingValue BtcAmount  -- ^ Insufficient funding value to pay server-defined "open price"

instance Show ChannelCreationError where
    show (FundingError e) = unwords
        [ "Funding error:"
        , show e
        ]
    show (InsufficientFundingValue val) = unwords
        [ "Insufficient funding value to pay server-defined 'open price' of"
        , show val
        ]

channelWithInitialPayment ::
       -- Monad m
       HC.PrvKeyC
    -- ^ Client private key. Its corresponding public key was provided to server when fetching 'RBPCP.FundingInfo'
    -> LockTimeDate
    -- ^ Channel expiration date (was also provided to the server when fetching 'RBPCP.FundingInfo')
    -> (HT.Tx, RBPCP.Vout)
    -- ^ Funding transaction, plus transaction output index to use
    -> RBPCP.FundingInfo
    -- ^ Server-returned funding info
    -> Either ChannelCreationError (ClientPayChanI BtcSig, SignedPayment)
    -- ^ Client state and first payment (or error).
channelWithInitialPayment prvKey expTime fundingTxVout fi@RBPCP.FundingInfo{..} =
    let mkPayChanState sp = MkPayChanState sp (fromInitialPayment sp) serverConf
        mkClientChan sp = (MkClientPayChan (mkPayChanState sp) prvKey, sp)
        serverConf = fromFundingInfo fi
        openPrice = fromIntegral fundingInfoOpenPrice
        cp = MkChanParams
                (MkSendPubKey $ HC.derivePubKey prvKey)
                (MkRecvPubKey . (\(RBPCP.Server a) -> a) $ fundingInfoServerPubkey)
                expTime
        unsignedPayE = fmapL FundingError $ runConfM serverConf $
              mkUnsignedPayment cp fundingTxVout (getP2SHFundingAddress cp)
        initialPayE = do
            unsignedPay <- unsignedPayE
            fmapL (const $ InsufficientFundingValue openPrice) $ runConfM serverConf $ createPaymentOfValue
                prvKey unsignedPay (fromIntegral fundingInfoOpenPrice)
    in
        mkClientChan <$> initialPayE

-- |Create new payment of specified value, along with updated state containing this payment.
createPayment :: forall value ret.
       PaymentValueSpec value ret BtcSig
    => ClientPayChanI BtcSig
    -- ^ Sender state object
    -> value
    -- ^ Amount to send (capped or uncapped)
    -> ret
    -- ^ Updated sender state & payment
createPayment = createPaymentInternal

-- |Create new payment of specified value, along with updated state containing this payment.
createPaymentInternal :: forall value ret sd.
       PaymentValueSpec value ret sd
    => ClientPayChanI sd
    -- ^ Sender state object
    -> value
    -- ^ Amount to send. Either exact ('BtcAmount') or capped ('Capped BtcAmount')
    -> ret
    -- ^ Updated sender state & payment
createPaymentInternal cpc@MkClientPayChan{..} payVal =
    let
        actualPayVal = paymentValue (serverConfDustLimit serverConf) cpc payVal
        serverConf = pcsSettings spcState
        paymentE = runConfM serverConf $ createPaymentOfValue
              spcPrvKey (clearSig $ pcsPayment spcState) actualPayVal
        updateState pcs p = pcs { pcsPayment = p }
        addUpdatedState p = (cpc { spcState = updateState spcState p }, p)
    in
        mkReturnVal (Tagged actualPayVal :: Tagged (value,sd) BtcAmount)
                    (addUpdatedState <$> paymentE)

-- TODO: cleanup
createClosingPayment
    :: (ChangeOutFee fee, HasFee fee )
    => ClientPayChanI BtcSig    -- ^ Client state
    -> HC.Address               -- ^ Client change address
    -> fee                      -- ^ Settlement transaction fee (which equals value of closing payment)
    -> (ClientPayChanI BtcSig, SignedPayment, BtcAmount)
createClosingPayment clientState changeAddress fee =
    createPayment newState (Capped $ absoluteFee 0 (dummySettleTxSize newState actualAmount) fee)
  where
    (newState, _, actualAmount) = createPaymentInternal
          newChangeAddrState (Capped $ absoluteFee 0 (dummySettleTxSize fakeSigNewAddrState (0 :: BtcAmount)) fee)

    dummySettleTxSize cpc' fee' = calcTxSize $ dummyClientSettleTx cpc' fee'
    newChangeAddrState = _setClientChangeAddr clientState changeAddress
    -- ### Dummy
    fakeSigNewAddrState = mapSigData _invalidBtcSig newChangeAddrState
    handleSettleRet (Identity (Right dummySettleTx)) = dummySettleTx
    handleSettleRet (Identity _) = error "woops"
    dummyAddress = HC.PubKeyAddress "0000000000000000000000000000000000000000"
    dummyClientSettleTx :: ChangeOutFee txFee => ClientPayChan -> txFee -> HT.Tx
    dummyClientSettleTx  cpc txFee = handleSettleRet $ getSettlementBitcoinTx
                          (dummyFromClientState cpc) dummyAddress (const $ return $ spcPrvKey cpc) txFee DropDust


-- |Produces a Bitcoin transaction which sends all channel funds back to the sender.
-- Will not be accepted by the Bitcoin network until the expiration time specified in
-- 'ChanParams'. Receiver should be aware of Bitcoin network time drift and the
-- unpreditable nature of finding new blocks.
getRefundBitcoinTx
    :: Monad m
    => HC.PrvKeyC
    -> ChanParams
    -> FundingTxInfo
    -> HC.Address           -- ^ Refund address
    -> SatoshisPerByte      -- ^ Refund transaction fee
    -- ^ Refund Bitcoin transaction.
    -- Error only in case of insufficient value to cover fee
    --  (dust outputs are accepted).
    -> m (Either BtcError HT.Tx)
getRefundBitcoinTx prvKey cp fti refundAddr txFee =
    fmap toHaskoinTx <$>
        mkRefundTx prvKey cp fti refundAddr txFee

{-# SPECIALIZE createPaymentInternal :: ClientPayChanI BtcSig -> BtcAmount -> Either BtcError (ClientPayChan, SignedPayment) #-}
{-# SPECIALIZE createPaymentInternal :: ClientPayChanI BtcSig -> Capped BtcAmount -> (ClientPayChan, SignedPayment, BtcAmount) #-}
