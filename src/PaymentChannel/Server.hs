module PaymentChannel.Server
( -- * 1. Check expiration date
  hasMinimumDuration
  -- * 2. Create state
, channelFromInitialPayment
  -- * 3. Accept payments
, acceptPayment
, acceptClosingPayment
  -- * 4. Settle
, closedGetSettlementTxSimple
, closedGetSettlementTxDerive
, getSignedSettlementTx
, SettleTx
) where

import PaymentChannel.Internal.Payment
import PaymentChannel.RBPCP.Parse
import PaymentChannel.Internal.Receiver.Util
import PaymentChannel.Internal.Receiver.Open    (initialServerState)
import PaymentChannel.Internal.Settlement
import PaymentChannel.Types
import PaymentChannel.Internal.Error

import qualified  Network.Haskoin.Crypto        as HC
import qualified  Network.Haskoin.Transaction   as HT
import Control.Monad.Trans.Either


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
    hoistEither . checkOpenPrice
        =<< hoistEither
        =<< acceptPaymentInternal paymentData initialState
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


-- |Get the settlement tx for a 'ClosedServerChanI', where the closing payment
--   pays the Bitcoin transaction fee
closedGetSettlementTxSimple ::
       PrvKeyC
    -> ClosedServerChanI a              -- ^ Produced by 'acceptClosingPayment'
    -> HC.Address                       -- ^ Receiver destination address. Funds sent over the channel will be sent to this address, the rest back to the client change address (an argument to 'channelWithInitialPaymentOf').
    -> SatoshisPerByte                  -- ^ Minimum transaction fee
    -> DustPolicy                       -- ^ Whether to keep or drop receiver change output if below dust limit
    -> Either ReceiverError SettleTx    -- ^ Settling Bitcoin transaction
closedGetSettlementTxSimple prvKey csc@MkClosedServerChan{..} recvAdr minFee dp = do
    (settleState,txFee) <- closedSettleInternalAcceptPay csc
    runSimple prvKey $ getSignedSettlementTx settleState (mkChangeOut txFee)
  where
    mkChangeOut fee = ChangeOut recvAdr (MaximumFee $ MaxFee (fee,minFee)) dp

-- | Same as 'closedGetSettlementTx', but allow more advanced signing types
closedGetSettlementTxDerive ::
       DeriveChangeOut SignedPayment (TxFee,DustPolicy) signKey ChanParams
    => ClosedServerChanI a
    -> SatoshisPerByte
    -> DustPolicy
    -> SignM signKey (Either ReceiverError SettleTx)
closedGetSettlementTxDerive csc@MkClosedServerChan{..} minFee dp = do
    let getSettleTx (settleState,txFee) = getSignedSettlementTx settleState (MaximumFee $ MaxFee (txFee,minFee), dp)
    either (return . Left) getSettleTx (closedSettleInternalAcceptPay csc)


closedSettleInternalAcceptPay
    :: ClosedServerChanI t
    -> Either ReceiverError (ServerPayChanI t, BtcAmount)
closedSettleInternalAcceptPay MkClosedServerChan{..} =
    fmapL BadClosedServerChan resE
  where
    resE = resultFromThePast $   -- Accept payments even for expired channels
        acceptClosingPaymentInternal (toPaymentData cscClosingPayment) cscState


{-# SPECIALIZE acceptPaymentInternal :: MonadTime m => PaymentData -> ServerPayChan -> m (Either PayChanError (ServerPayChan, BtcAmount)) #-}
{-# SPECIALIZE acceptPaymentInternal :: MonadTime m => PaymentData -> ServerPayChanX -> m (Either PayChanError (ServerPayChanX, BtcAmount)) #-}
