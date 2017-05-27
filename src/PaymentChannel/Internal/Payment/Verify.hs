module PaymentChannel.Internal.Payment.Verify
( paymentValueIncrease
, StateSignature
, _invalidBtcSig
)
where

import PaymentChannel.Internal.Payment.Types as Export
import PaymentChannel.Internal.Error.User
import PaymentChannel.Internal.Error.Internal         (ReceiverError(BadSignatureInState))
import Bitcoin.Compare

import Control.Exception                              (throw)
import Debug.Trace


-- | When we don't have a valid signature for the 'Payment' in the state
--    its signature data will be an 'InvalidSig'. Conversion
--    to a BtcSig is for backwards compatibility with 'payValIncrease' etc.
class StateSignature a where
    checkStateSig  :: Payment a -> Either VerifyError ()
    _btcSigPossiblyFake :: Payment a -> Payment BtcSig    -- ^ For backwards compatibilty with 'payValIncrease' (which doesn't look at signature data)

instance StateSignature BtcSig where
    checkStateSig = singlePairVerifySig 
    _btcSigPossiblyFake = id

instance StateSignature InvalidSig where
    checkStateSig = const $ Right () 
    _btcSigPossiblyFake = mapSigData _invalidBtcSig
        
-- | WARNING: Produces invalid 'BtcSig'
_invalidBtcSig :: InvalidSig -> BtcSig
_invalidBtcSig (MkInvalidSig sh) = MkBtcSig dummySig sh

-- | Throws 'BadSignatureInState' on invalid in-state payment signature
paymentValueIncrease :: 
       ( MonadTime m
       , StateSignature stateSigData
       ) =>
       PayChanState stateSigData  -- ^ State with old payment
    -> Payment BtcSig             -- ^ New payment
    -> m (Either PayChanError BtcAmount)
paymentValueIncrease state newPayment = do
    let settlePeriod = runConfM (pcsSettings state) confSettlePeriod
    fundingLocked <- fundingIsLocked (toSeconds settlePeriod) newPayment
    return $ 
        if fundingLocked 
            then checkedPayVal (pcsPayment state) newPayment
            else Left ChannelExpired
  where
    checkedPayVal statePayment payment = do
            valRecvd <- payValIncrease (_btcSigPossiblyFake statePayment) payment
            fmapL (const $ throw BadSignatureInState) (checkStateSig statePayment)
            _ <- fmapL (const SigVerifyFailed) (singlePairVerifySig payment)
            return valRecvd

payValIncrease ::
       Payment BtcSig    -- ^ Old, in-state payment
    -> Payment BtcSig    -- ^ New payment
    -> Either PayChanError BtcAmount
payValIncrease sp1 sp2 =
    comparePayments sp1 sp2 >>=
    \res -> case res of
        -- Inspect change in client change value.
        -- A decrease in client change value is an increase in value to us.
        DiffInfo [(_, Decrease val)] -> Right val
        DiffInfo [(_, NoChange)]     -> Right 0
        DiffInfo [(_, Increase val)] -> Left $ BadPaymentValue val
        DiffInfo x -> error $ "Not exactly one output in a 'Payment': " ++ show x

comparePayments ::
       Payment BtcSig
    -> Payment BtcSig
    -> Either PayChanError DiffInfo
comparePayments sp1 sp2 =
    fmapL PaymentError (valueDiff tx1 tx2) >>= eqIgnoreVal
  where
    (tx1,tx2) = (toBtcTx sp1, toBtcTx sp2)
    eqIgnoreVal di =
        if not $ eqIgnoreOutVal (IgnoreSigData tx1) (IgnoreSigData tx2)
            -- Means sig hash flags differ
            then Left $ show (tx1,tx2) `trace` BadSigHashFlag (bsSigFlag $ getSigData sp2) (bsSigFlag $ getSigData sp1)
            else Right di
