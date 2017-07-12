module PaymentChannel.Internal.Payment.Create
( mkUnsignedPayment
, createPaymentOfValue
, CreationError
, module Export
)
where

import PaymentChannel.Internal.Payment.Types    as Export


import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import qualified Network.Haskoin.Script                   as HS
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
--    , SignatureScript t r BtcSig
    , SpendFulfillment BtcSig r
--    , HasSpendCond r t
    , Show r
    ) =>
       HC.PrvKeyC
    -> SigSinglePair t r ()
    -> BtcAmount
    -> m (Either BtcError (SigSinglePair t r BtcSig))
createPaymentOfValue prvKey ssp payVal = do
    newSspE <- decrementClientValue ssp payVal
    either (return . Left) (signPair prvKey) newSspE

decrementClientValue ::
    HasConfDustLimit m
    => SigSinglePair t r ()
    -> BtcAmount
    -> m (Either BtcError (SigSinglePair t r ()))
decrementClientValue sp@SigSinglePair{..} decVal = do
    newValE <- mkNonDusty (currentVal - decVal)
    let newSignFlag newVal = if newVal /= nullAmount then HS.SigSingle True else HS.SigNone True
        newIn newVal = setSignFlag (newSignFlag newVal) singleInput
        mkNewPair newVal = sp { singleOutput = replaceValue singleOutput newVal
                              , singleInput  = newIn newVal
                              }
    return $ mkNewPair <$> newValE
  where
    currentVal = nonDusty $ btcAmount singleOutput
    replaceValue out val = out { btcAmount = val }
