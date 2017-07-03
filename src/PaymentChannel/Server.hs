module PaymentChannel.Server
( hasMinimumDuration
, channelFromInitialPayment
, acceptPayment
, acceptClosingPayment
, getSettlementBitcoinTx
, closedGetSettlementTx
, SettleTx
) where

import PaymentChannel.Internal.Payment
import PaymentChannel.RBPCP.Parse
import PaymentChannel.Internal.Receiver.Util
import PaymentChannel.Internal.Receiver.Open    (initialServerState)
import PaymentChannel.Internal.Settlement
import PaymentChannel.Types
import PaymentChannel.Internal.Error
import Control.Exception                        (throw)

import qualified  Network.Haskoin.Crypto        as HC
import qualified  Network.Haskoin.Transaction   as HT
import Control.Monad.Trans.Either
-- import Control.Monad.Trans.Class                (lift)


hasMinimumDuration ::
       ( HasLockTimeDate lockTime, MonadTime m )
    => ServerSettings
    -> lockTime
    -> m (Either OpenError ())
hasMinimumDuration ServerSettings{..} lt = do
    hasMinDuration <- isLocked (toSeconds $ serverConfSettlePeriod + serverConfMinDuration) lt
    return $ if hasMinDuration
        then Right ()
        else Left $ InsufficientDuration serverConfMinDuration

-- |Create new 'ServerPayChan'
channelFromInitialPayment ::
       MonadTime m
    => ServerSettings
       -- ^ Derived from/matches the client's 'RBPCP.Types.FundingInfo'
    -> HT.Tx
       -- ^ Funding transaction
    -> PaymentData
       -- ^ Opening payment produced by client ('PaymentChannel.Client.channelWithInitialPayment')
    -> m (Either PayChanError ServerPayChan)
       -- ^ Server channel state
channelFromInitialPayment cfg@ServerSettings{..} tx paymentData = runEitherT $ do
    initialState <- hoistEither $ fmapL OpenError $ initialServerState cfg tx paymentData
    hoistEither =<< fmapL OpenError <$> hasMinimumDuration cfg initialState
    hoistEither . checkOpenPrice =<< hoistEither =<< acceptPaymentInternal paymentData initialState
  where
    checkOpenPrice (state,val) =
        if val /= serverConfOpenPrice
            then Left $ OpenError $ IncorrectInitialPaymentValue $ val `FoundButExpected` serverConfOpenPrice
            else Right state

-- | Register, on the receiving side, a payment made by 'createPayment' on the sending side.
--   Returns error if either the signature or payment amount is invalid, and otherwise
--    the amount received with this 'Payment' and a new state object.
-- | NB: Throws 'BadSignatureInState' on invalid old/in-state payment.
acceptPayment :: MonadTime m =>
       PaymentData          -- ^Payment to verify and register
    -> ServerPayChanI kd    -- ^Receiver state object
    -> m (Either PayChanError (ServerPayChanI kd, BtcAmount)) -- ^Value received plus new receiver state object
acceptPayment = acceptPaymentInternal

acceptPaymentInternal ::
      ( MonadTime m
      , StateSignature sd
      ) =>
       PaymentData            -- ^Payment to verify and register
    -> ServerPayChanG kd sd   -- ^Receiver state object
    -> m (Either PayChanError (ServerPayChanI kd, BtcAmount)) -- ^Value received plus new receiver state object
acceptPaymentInternal paymentData rpc =
    either (return . Left) (acceptPaymentIgnoreStatus paymentData) (checkChannelStatus rpc)

-- | Accept the payment that closes the payment channel.
--   The payment accepted here is allowed to have a different client change address
--    from that found in the state.
-- | NB: Throws 'BadSignatureInState' on invalid old/in-state payment.
acceptClosingPayment :: MonadTime m =>
       PaymentData         -- ^Payment to verify and register
    -> ServerPayChanI kd    -- ^Receiver state object
    -> m (Either PayChanError (ClosedServerChanI kd))
acceptClosingPayment paymentData oldState =
    fmap handleResult <$> acceptClosingPaymentInternal paymentData oldState
  where
    mkNewStatus newState = ChannelClosed $ getPayment newState
    getPayment = pcsPayment . rpcState
    handleResult (newState, _) = MkClosedServerChan
            (setChannelStatus (mkNewStatus newState) oldState)
            (getPayment newState)

acceptPaymentIgnoreStatus ::
      ( MonadTime m
      , StateSignature sd
      ) =>
       PaymentData              -- ^Payment to verify and register
    -> ServerPayChanG kd sd     -- ^Receiver state object
    -> m (Either PayChanError (ServerPayChanG kd BtcSig, BtcAmount)) -- ^Value received plus new receiver state object
acceptPaymentIgnoreStatus paymentData rpc@MkServerPayChan{..} =
    either (return . Left) checkPayment (getPayment paymentData)
  where
    serverConf = pcsSettings rpcState
    getPayment pd = fmapL RBPCPError $ runConfM serverConf $ fromPaymentData (getFundingAmount rpc) pd
    mkReturnValue p val = (updateMetadata $ updState rpc p, val)
    checkPayment p = do
            valRecvdE <- paymentValueIncrease rpcState p
            return $ mkReturnValue p <$> valRecvdE

acceptClosingPaymentInternal ::
       MonadTime m
    => PaymentData        -- ^Payment to verify and register
    -> ServerPayChanI kd   -- ^Receiver state object
    -> m (Either PayChanError (ServerPayChanI kd, BtcAmount))
acceptClosingPaymentInternal paymentData oldState =
    acceptPaymentIgnoreStatus paymentData newChangeAddrState
  where
    newChangeAddrState = _setClientChangeAddr oldState (paymentDataChangeAddress paymentData)


-- |The value transmitted over the channel is settled when this transaction is in the Blockchain.
-- The receiver will want to make sure a transaction produced by this function
-- is included in a Bitcoin block before the refund transaction becomes valid (see 'getRefundBitcoinTx').
-- The sender can only close the channel before expiration by requesting this transaction
-- from the receiver and publishing it to the Bitcoin network.
getSettlementBitcoinTx ::
     ( HasSigningKey prvKey P2SH ChanParams BtcSig
     , ChangeOutFee fee
     ) =>
       ServerPayChanI kd                            -- ^ Receiver state object
    -> HC.Address                                   -- ^ Receiver destination address. Funds sent over the channel will be sent to this address, the rest back to the client change address.
    -> fee                                          -- ^ Bitcoin transaction fee
    -> DustPolicy                                   -- ^ Whether to keep or drop receiver change output if below dust limit
    -> SignM prvKey (Either ReceiverError SettleTx)    -- ^ Settling Bitcoin transaction
getSettlementBitcoinTx rpc recvAdr txFee dp =
    getSignedSettlementTx rpc (mkChangeOut recvAdr txFee dp)

-- |Get the settlement tx for a 'ClosedServerChanI', where the closing payment
--   pays the Bitcoin transaction fee
closedGetSettlementTx ::
       ( HasSigningKey prvKey P2SH ChanParams BtcSig
       )
    => ClosedServerChanI a              -- ^ Produced by 'acceptClosingPayment'
    -> HC.Address                       -- ^ Receiver destination address. Funds sent over the channel will be sent to this address, the rest back to the client change address (an argument to 'channelWithInitialPaymentOf').
    -> DustPolicy                       -- ^ Whether to keep or drop receiver change output if below dust limit
    -> SignM prvKey (Either ReceiverError SettleTx)   -- ^ Settling Bitcoin transaction
closedGetSettlementTx MkClosedServerChan{..} recvAdr dp = do
    let resE  = resultFromThePast $ acceptClosingPaymentInternal (toPaymentData cscClosingPayment) cscState
        (settleState,txFee) = either (throw . BadClosedServerChan) id resE
    getSettlementBitcoinTx settleState recvAdr txFee dp

{-# SPECIALIZE acceptPaymentInternal :: MonadTime m => PaymentData -> ServerPayChan -> m (Either PayChanError (ServerPayChan, BtcAmount)) #-}
{-# SPECIALIZE acceptPaymentInternal :: MonadTime m => PaymentData -> ServerPayChanX -> m (Either PayChanError (ServerPayChanX, BtcAmount)) #-}
