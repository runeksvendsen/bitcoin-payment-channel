module PaymentChannel.Internal.Payment.Create
(
  mkUnsignedPayment
, createPaymentOfValue
, CreationError
, module Export
)
where

import PaymentChannel.Internal.Payment.Types    as Export


import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import Bitcoin.SpendCond.Util                   (singlePrevIn, PickOutError)
-- import Bitcoin.Dust                             (getDustLimit)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class                (lift)


data CreationError
  = BadFundingOutput (PickOutError ChanParams)
  | DustyFundingAmount BtcAmount
      deriving Show   -- TODO

mkUnsignedPayment ::
    HasConfDustLimit m
    => ChanParams
    -> (HT.Tx, Word32)
    -> HC.Address
    -> m (Either CreationError UnsignedPayment)
mkUnsignedPayment cp (tx,idx) refundAddr = runEitherT $ do
    input      <- hoistEither $ fmapL BadFundingOutput $ singlePrevIn tx cp idx
    dustLimit  <- lift confDustLimit
    fundingVal <- hoistEither . fmapL (const $ DustyFundingAmount dustLimit)
                    =<< lift (mkNonDusty $ btcInValue input)
    return $ mkSigSinglePair input (mkBtcOut refundAddr fundingVal)


createPaymentOfValue ::
    ( HasConfDustLimit m
    , TransformSigData BtcSig () r
    , SignatureScript t BtcSig
    , SpendFulfillment BtcSig r
    , HasSpendCond r t
    , Show t
    ) =>
       HC.PrvKeyC
    -> SigSinglePair t ()
    -> BtcAmount
    -> m (Either BtcError (SigSinglePair t BtcSig))
createPaymentOfValue prvKey ssp payVal = do
    newSspE <- decrementClientValue ssp payVal
    either (return . Left) (signPair prvKey) newSspE

decrementClientValue ::
    HasConfDustLimit m
    => SigSinglePair r ()
    -> BtcAmount
    -> m (Either BtcError (SigSinglePair r ()))
decrementClientValue sp@SigSinglePair{..} decVal = do
    newValE <- mkNonDusty (currentVal - decVal)
    return (newValE >>= \newVal -> Right $ sp { singleOutput = replaceValue singleOutput newVal })
  where
    currentVal = nonDusty $ btcAmount singleOutput
    replaceValue out val = out { btcAmount = val }











