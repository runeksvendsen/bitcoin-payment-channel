module PaymentChannel.Client
( validFundingInfo
, channelWithInitialPayment
, createPayment
, createPaymentCapped
, createClosingPayment
, getRefundBitcoinTx
, RefundTx
)
where

import PaymentChannel.Server                            (getSignedSettlementTx)
import PaymentChannel.Internal.Payment
import PaymentChannel.Internal.Receiver.Util
import Bitcoin.Util                                     (calcTxSize)
import PaymentChannel.Internal.Refund                   (RefundTx, mkRefundTx)
import PaymentChannel.Types
import Data.Functor.Identity                            (Identity(..))
import Control.Exception                                (Exception, throw)
import qualified RBPCP.Types                            as RBPCP
import qualified  Network.Haskoin.Crypto                as HC
import qualified  Network.Haskoin.Transaction           as HT


data ChannelCreationError
  = FundingError CreationError          -- ^ Invalid funding output or dusty funding amount
  | InsufficientFundingValue BtcAmount  -- ^ Insufficient funding value to pay server-defined "open price"
  | BadServerFundingAddress (ExpectFail HC.Address)

instance Exception ChannelCreationError
instance Show ChannelCreationError where
    show (FundingError e) = unwords
        [ "Funding error:"
        , show e
        ]
    show (InsufficientFundingValue val) = unwords
        [ "Insufficient funding value to pay server-defined 'open price' of"
        , show val
        ]
    show (BadServerFundingAddress expec) = unwords
        [ "Disagreement on channel funding address.", show expec ]

validFundingInfo
    :: SendPubKey
    -> LockTimeDate
    -> RBPCP.FundingInfo
    -> Either ChannelCreationError ChanParams
validFundingInfo sendPK lockTime RBPCP.FundingInfo{..} =
    if correctAddress /= serversAddress
        then Left $ BadServerFundingAddress $ serversAddress `FoundButExpected` correctAddress
        else Right chanParams
  where
    serversAddress = fundingInfoFundingAddressCopy
    getServerKey (RBPCP.Server pk) = pk
    correctAddress = getP2SHFundingAddress chanParams
    chanParams = ChanParams
            sendPK
            (MkRecvPubKey $ getServerKey fundingInfoServerPubkey)
            lockTime

channelWithInitialPayment
    :: HC.PrvKeyC
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
    let serverConf = fromFundingInfo fi
        mkPayChanState sp = MkPayChanState sp (fromInitialPayment sp) serverConf
        mkClientChan sp = (MkClientPayChan (mkPayChanState sp) prvKey, sp)
        openPrice = fromIntegral fundingInfoOpenPrice
        clientPK = MkSendPubKey $ HC.derivePubKey prvKey
    in do
        cp <- validFundingInfo clientPK expTime fi
        unsignedPay <- fmapL FundingError $ runConfM serverConf $
                          mkUnsignedPayment cp fundingTxVout (getP2SHFundingAddress cp)
        initialPay <- fmapL (const $ InsufficientFundingValue openPrice) $
                          createPaymentInternal unsignedPay serverConf prvKey (openPrice :: BtcAmount)
        Right $ mkClientChan initialPay

-- |Create new payment of specified value, along with updated state containing this payment.
createPayment ::
       ClientPayChan
    -- ^ Sender state object
    -> BtcAmount
    -- ^ Amount to send
    -> Either BtcError (ClientPayChan, SignedPayment)
    -- ^ Updated sender state & payment
createPayment cpc@MkClientPayChan{..} val = do
    payment <- createPaymentInternal (pcsPayment spcState) (pcsSettings spcState) spcPrvKey val
    Right (updateClientState cpc payment, payment)

updateClientState :: ClientPayChan -> SignedPayment -> ClientPayChan
updateClientState cpc@MkClientPayChan{..} payment =
    cpc { spcState = updatePcs payment }
  where
    updatePcs p = spcState { pcsPayment = p }


-- |Create new payment of, at most, specified value, along with updated state containing this payment.
createPaymentCapped ::
       ClientPayChan
    -- ^ Sender state object
    -> Capped BtcAmount
    -- ^ Maximum amount to send
    -> (ClientPayChan, SignedPayment, BtcAmount)
    -- ^ Updated sender state, payment and actual payment value
createPaymentCapped = createPaymentCappedInternal

-- | Same as 'createPaymentCapped', but accepts a client state without a signature
createPaymentCappedInternal ::
       ClientPayChanI sd
    -> Capped BtcAmount
    -> (ClientPayChan, SignedPayment, BtcAmount)
createPaymentCappedInternal cpc@MkClientPayChan{..} cappedVal =
    let
        newPayment = failOnBug $ createPaymentInternal
            (pcsPayment spcState) (pcsSettings spcState) spcPrvKey cappedVal
        failOnBug (Right newPay) = newPay
        failOnBug (Left e) = error $ "BUG: createPaymentCapped: capped amount math fail: " ++ show e
        realVal = clientChangeVal (pcsPayment spcState) - clientChangeVal newPayment
        updatePcs   p = spcState { pcsPayment = p }
        updateState p = cpc { spcState = updatePcs p }
    in
    (updateState newPayment, newPayment, realVal)

-- |Create new payment of specified value, along with updated state containing this payment.
createPaymentInternal ::
       PaymentValueSpec value
    => Payment sd
    -- ^ Sender state object
    -> ServerSettings
    -> HC.PrvKeyC
    -- ^ Sender private key
    -> value
    -- ^ Amount to send. Either exact ('BtcAmount') or capped ('Capped BtcAmount')
    -> Either BtcError SignedPayment
    -- ^ Payment (+ payment value)
createPaymentInternal oldPayment servSettings prvKey payVal =
    let
        actualPayVal = paymentValue (clientChangeVal oldPayment) servSettings payVal
        paymentE = runConfM servSettings $ createPaymentOfValue
              prvKey (clearSig oldPayment) actualPayVal
        -- BUG: Fail if we supplied the wrong private key to the signing function
        --  (pubkey derived from private key does match client pubkey in 'ChanParams')
        checkSignErr e = if isKeyError e then throw $ Bug e else e
    in
        fmapL checkSignErr paymentE

-- TODO: cleanup
createClosingPayment
    :: (ToChangeOutFee fee, HasFee fee )
    => ClientPayChanI BtcSig    -- ^ Client state
    -> HC.Address               -- ^ Client change address
    -> fee                      -- ^ Settlement transaction fee (which equals value of closing payment)
    -> (ClientPayChanI BtcSig, SignedPayment, BtcAmount)
        -- ^ Closed state; closing payment; actual fee (capped to remaining channel value)
createClosingPayment clientState changeAddress fee =
    createPaymentCapped newState (Capped $ absoluteFee 0 (dummySettleTxSize newState actualAmount) fee)
  where
    (newState, _, actualAmount) = createPaymentCappedInternal
          newChangeAddrState (Capped $ absoluteFee 0 (dummySettleTxSize fakeSigNewAddrState (0 :: BtcAmount)) fee)

    dummySettleTxSize cpc' fee' = calcTxSize $ dummyClientSettleTx cpc' fee'
    newChangeAddrState = _setClientChangeAddr clientState changeAddress
    -- ### Dummy
    fakeSigNewAddrState = mapSigData _invalidBtcSig newChangeAddrState
    handleSettleRet = either (error . ("createClosingPayment. woops: " ++) . show) id
    dummyClientSettleTx :: ToChangeOutFee txFee => ClientPayChan -> txFee -> HT.Tx
    dummyClientSettleTx  cpc txFee = toHaskoinTx . handleSettleRet . runDummy $ getSignedSettlementTx
                          (dummyFromClientState cpc) (mkChangeFee txFee, DropDust)


-- |Produces a Bitcoin transaction which sends all channel funds back to the sender.
-- Will not be accepted by the Bitcoin network until the expiration time specified in
-- 'ChanParams'. Receiver should be aware of Bitcoin network time drift and the
-- unpreditable nature of finding new blocks.
getRefundBitcoinTx
    :: ( Monad m, ToChangeOutFee fee )
    => ClientPayChan
    -> HC.Address
      -- ^Refund address
    -> fee
      -- ^Refund transaction fee
    -> m (Either BtcError RefundTx)
      -- ^ Refund Bitcoin transaction.
      -- Error only in case of insufficient value to cover fee
      --  (dust outputs are accepted).
getRefundBitcoinTx =
    mkRefundTx

{-# SPECIALIZE createPaymentInternal :: Payment BtcSig -> ServerSettings -> HC.PrvKeyC -> BtcAmount -> Either BtcError SignedPayment #-}
{-# SPECIALIZE createPaymentInternal :: Payment BtcSig -> ServerSettings -> HC.PrvKeyC -> Capped BtcAmount -> Either BtcError SignedPayment #-}
