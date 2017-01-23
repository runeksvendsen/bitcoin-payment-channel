module PaymentChannel.Internal.Payment.Verify
(
  paymentValueIncrease
)
where


import PaymentChannel.Internal.Payment.Types as Export
import PaymentChannel.Internal.Error.User

-- import RBPCP.Types
import PaymentChannel.Internal.RBPCP.Parse
import PaymentChannel.Internal.Types
import PaymentChannel.Internal.ChanScript
import Bitcoin.Util
import Bitcoin.SinglePair
import Bitcoin.Compare
import PaymentChannel.Internal.Util

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import qualified Data.List.NonEmpty     as NE


paymentValueIncrease :: MonadTime m =>
       Payment BtcSig
    -> Payment BtcSig
    -> m (Either PayChanError BtcAmount)
paymentValueIncrease sp1 sp2 = do
    fundingLocked <- fundingIsLocked sp2
    if fundingLocked then return checkedPayVal else return $ Left ChannelExpired
  where
    checkedPayVal = do
            valRecvd <- payValIncrease sp1 sp2
            _ <- fmapL (const SigVerifyFailed) (singlePairVerifySig sp2)
            return valRecvd

payValIncrease ::
       Payment BtcSig
    -> Payment BtcSig
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
            then Left BadSigHashFlag  -- Means sig hash flags differ
            else Right di
