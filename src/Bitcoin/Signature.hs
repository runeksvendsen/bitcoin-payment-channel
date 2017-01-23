module Bitcoin.Signature
(
  module Bitcoin.Conversion
, TransformSigData(..)
, signTx, signSettleTx
, verifyTx
)
where

import Bitcoin.Conversion
import Bitcoin.Util

import           Data.Word              (Word32)
import           Control.Monad          (zipWithM)
import qualified Data.List.NonEmpty     as NE
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC


class SpendCondition r => TransformSigData newSd oldSd r where
    mkSigData :: oldSd -> BtcSig -> r -> newSd





-- | Sign transaction
signTx :: forall t r ss oldSd m.
          (Monad m, TransformSigData ss oldSd r, SignatureScript t ss
          , SpendFulfillment ss r, HasSpendCond r t) =>
          (KeyDeriveIndex -> m HC.PrvKeyC)
       -> BtcTx t oldSd
       -> m (Either BtcError (BtcTx t ss))
signTx signFunc tx@BtcTx{..} = signReplaceInputs signFunc tx

-- | Sign transaction with added inputs and change output
signSettleTx :: forall t r ss oldSd m.
              ( Monad m
              , HasSpendCond r t, SpendFulfillment ss r, TransformSigData ss oldSd r, SignatureScript t ss
              ) =>
          (KeyDeriveIndex -> m HC.PrvKeyC)
       -> ChangeOut
       -> BtcTx t oldSd
       -> m (Either BtcError (BtcTx t ss))
signSettleTx signFunc chgOut tx@BtcTx{..} = mkRelativeFeeTxM (btcTxFee chgOut) mkTx
        where mkTx fee = signReplaceInputs signFunc (setTxRawFee fee tx)

signReplaceInputs :: forall t r ss oldSd m.
          ( Monad m
          , TransformSigData ss oldSd r
          , SignatureScript t ss
          , SpendFulfillment ss r
          , SpendCondition r
          , HasSpendCond r t
          ) =>
          (KeyDeriveIndex -> m HC.PrvKeyC)
       -> BtcTx t oldSd
       -> m (Either BtcError (BtcTx t ss))
signReplaceInputs f tx =
    if availableVal tx >= 0
        then signInputs f tx >>= \ins -> return $ Right tx { btcIns = ins }
        else return . Left . InsufficientFunds . fromIntegral . abs . availableVal $ tx

signInputs :: forall t r ss oldSd m.
              ( Monad m
              , TransformSigData ss oldSd r
              , SignatureScript t ss
              , SpendFulfillment ss r
              , SpendCondition r
              , HasSpendCond r t
              ) =>
              (KeyDeriveIndex -> m HC.PrvKeyC)
           -> BtcTx t oldSd
           -> m (NE.NonEmpty (InputG t ss))
signInputs getKey tx@BtcTx{..} =
    unsafeCastNE <$> zipWithM signIt [0..] (NE.toList btcIns)
  where
    signIt idx inp@MkInputG{..} = do
             prv <- getKey btcKeyIndex
             let rdmScr = getCond btcInType :: r
             let rawSig = getHashForSig tx rdmScr idx btcSignFlag `HC.signMsg` prv
             let sigData = mkSigData btcSigData (MkBtcSig rawSig btcSignFlag) rdmScr
             return $ mapSigData (const sigData) inp

verifyTx :: (SpendFulfillment ss r, SpendCondition r, HasSpendCond r t) =>
                BtcTx t ss -> Either VerifyError ()
verifyTx tx@BtcTx{..} =
    if null verifyRes then Right () else Left $ SigVerifyFail $ map snd verifyRes
  where
    verifyRes = concatMap getErrors $ zipWith (verifyInput tx) [0..] (NE.toList btcIns)
    getErrors = filter ((== False) . fst)

verifyInput :: forall r t ss.
               (SpendFulfillment ss r, SpendCondition r, HasSpendCond r t) =>
                  BtcTx t ss
               -> Word32
               -> InputG t ss
               -> [(Bool, (Word32, PubKey, HC.Hash256, HC.Signature))]
verifyInput tx idx MkInputG{..} = do
         let redeemScr = getCond btcInType :: r
         let getHash = getHashForSig tx redeemScr idx
         let keySigL = rawSigs btcSigData redeemScr
         let sigVerify (pk, MkBtcSig sig flag) =
                ( HC.verifySig (getHash flag) sig pk
                , (idx, pk, getHash flag, sig)
                )
         map sigVerify keySigL

getHashForSig ::
    SpendCondition r => BtcTx t a -> r -> Word32 -> HS.SigHash -> HC.Hash256
getHashForSig tx rdmScr idx = HS.txSigHash
    (toUnsignedTx tx) (conditionScript rdmScr) (toInt idx)


txSize :: SignatureScript t ss => BtcTx t ss -> TxByteSize
txSize = calcTxSize . toHaskoinTx

mkRelativeFeeTxM
    :: (Monad m, HasFee fee, SignatureScript t ss, HasSpendCond r t, SpendFulfillment ss r)
    => fee                                          -- ^Desired (per-byte) transaction fee
    -> ( BtcAmount -> m (Either e (BtcTx t ss)) )   -- ^Produces desired Bitcoin tx with given fee
    -> m (Either e (BtcTx t ss))
mkRelativeFeeTxM fee mkTxFunc =
    mkTxFunc (0 :: BtcAmount) >>= \txE ->
        case txE of
            Right tx -> mkTxSizeFee tx
            left     -> return left
    where
        mkTxSizeFee tx = mkTxFunc $ absoluteFee (txSize tx) fee


