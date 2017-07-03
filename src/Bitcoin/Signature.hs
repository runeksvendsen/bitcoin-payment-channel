{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bitcoin.Signature
( -- * Interface to fill out
  TransformSigData(..)
, HasSigner(..)
  -- * Provided functions: sign tx, verify tx
, signTx
, signChangeTx
, verifyTx
  -- * Runners
, runSimple
, runExtDet
, runDummy
, SignM
, SignSimpleM
, SignDerivM
, SignDummyM
, HasSigningKey
  -- * Re-exports
, module Bitcoin.Conversion
, module X
)
where

import Bitcoin.Conversion
import Bitcoin.Util
import Bitcoin.Internal.Util
import Bitcoin.BIP32.DetDerive

import Data.Default.Class               as X (Default (def))

import           Data.Word              (Word32)
import           Control.Monad          (zipWithM)
import qualified Data.List.NonEmpty     as NE
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC
import qualified Control.Monad.Reader   as R
--import Debug.Trace


-- | Identifies a signer whose signature produces newSigData
class SpendCondition r => HasSigner newSigData r where
    signerPubKey :: r -> Tagged newSigData HC.PubKeyC

-- -- | Defines which 'HS.SigHash' flag to use when signing an input from 'oldSigData'
--class HasSignFlag oldSigData where
--    getSignFlag :: oldSigData -> HS.SigHash

-- | Defines how to transform old signature data type into a new one (by adding signatures)
class (HasSigner newSigData r) --, HasSignFlag oldSigData)
        => TransformSigData newSigData oldSigData r | newSigData oldSigData -> r where
    mkSigData :: oldSigData   -- ^ Old signature data, needs next 'BtcSig' added to it
              -> BtcSig       -- ^ Signature produced by signing input
              -> Tagged r newSigData




-- ##############
-- ### Internal interfaces: abstract over source private key types
class Monad m => MonadSign m signKey | m -> signKey where
    signGetKey    :: m signKey
    getSignConf   :: m SignConf

class HasSigningKey key t r oldSigData where
    getSignKey :: InputG t r oldSigData -> key -> HC.PrvKeyC

-- Simple
instance HasSigningKey HC.PrvKeyC t r oldSig where
    getSignKey _ = id
-- BIP32+Deterministic derivation
instance DerivationSeed r => HasSigningKey RootPrv t r oldSig where
    getSignKey MkInputG{..} key =
        getKey (detDerive key btcCondScr :: External ChildPair)
-- Dummy
instance HasSigningKey () t r oldSig where
    getSignKey inp _ = dummyPrvKey inp

-- | Generic signing monad. Run with e.g. 'runSimple'
newtype SignM key a = SignM { getSignM :: R.Reader (SignData key) a }
    deriving (Functor, Applicative, Monad, R.MonadReader (SignData key))

type SignSimpleM = SignM HC.PrvKeyC

type SignDerivM = SignM RootPrv

type SignDummyM = SignM ()

-- |
newtype SignConf
  = SignConf
  { doSignCheck   :: Bool             -- ^ (Default: True) When signing, check whether signing private key's pubkey matches specified script pubkey
  }

instance Default SignConf where
    def = SignConf
          { doSignCheck   = True }

-- |
data SignData kd
  = SignData
  { sdKey  :: kd          -- ^ Private key data
  , sdConf :: SignConf
  }

instance MonadSign (SignM key) key where
    signGetKey = R.asks sdKey
    getSignConf = R.asks sdConf
--instance MonadSign SignDerivM RootPrv where
--    signGetKey = R.asks sdKey
--    getSignConf = R.asks sdConf
--instance MonadSign SignDummyM () where
--    signGetKey = R.asks sdKey
--    getSignConf = R.asks sdConf


-- | Run 'SignM' using a 'HC.PrvKeyC' private key
runSimple
    :: HC.PrvKeyC
    -> SignSimpleM a
    -> a
runSimple key =
    (`R.runReader` SignData key def) . getSignM

-- | Run using a BIP-32 extended root private key as source key,
--    with determinisitic key derivation.
runExtDet
    :: RootPrv
    -> SignDerivM a
    -> a
runExtDet rootKey =
    (`R.runReader` SignData rootKey def) . getSignM

-- | TEST: Run using dummy private key. Used e.g. for producing transactions
--    to test serialization length, for calculating tx fees relative to tx size.
runDummy
    :: SignDummyM a
    -> a
runDummy =
    (`R.runReader` SignData () noSigCheck) . getSignM
  where
    noSigCheck = SignConf { doSignCheck = False }



signTx :: forall m t r newSigData oldSd signKey.
              ( TransformSigData newSigData oldSd r
              , MonadSign m signKey
              , HasSigningKey signKey t r oldSd
              ) =>
              BtcTx t r oldSd
           -> m (Either BtcError (BtcTx t r newSigData))
signTx tx =
    if availableVal tx < 0
        then return . Left . InsufficientFunds . fromIntegral . abs . availableVal $ tx
        else do
            insE <- signInputs tx
            let replaceTxIns ins = tx { btcIns = ins }
                replacedIns = replaceTxIns <$> fmapL WrongSigningKey insE
            return replacedIns

signChangeTx :: forall m t r newSd oldSd signKey.
              ( SignatureScript r newSd t
              , TransformSigData newSd oldSd r
              , MonadSign m signKey
              , HasSigningKey signKey t r oldSd
              ) =>
              BtcTx t r oldSd
           -> ChangeOut
           -> m (Either BtcError (BtcTx t r newSd))
signChangeTx tx@BtcTx{..} chgOut =
    mkRelFeeFunc mkTx
 where
    mkTx :: BtcAmount -> m (Either BtcError (BtcTx t r newSd))
    mkTx fee = signTx (txWithChange fee)
    txWithChange :: BtcAmount -> BtcTx t r oldSd
    txWithChange fee = setTxRawFee fee $ setChangeOut chgOut tx
    mkRelFeeFunc :: (BtcAmount -> m (Either BtcError (BtcTx t r newSd)))
                -> m (Either BtcError (BtcTx t r newSd))
    mkRelFeeFunc = absOrRelFee mkRelativeFeeTxM mkRelativeFeeTxM (btcTxFee chgOut)


signInputs :: forall m t r newSigData oldSd signKey.
              ( TransformSigData newSigData oldSd r
              , MonadSign m signKey
              , HasSigningKey signKey t r oldSd
              )
           => BtcTx t r oldSd
           -> m (Either [SignKeyError] (NE.NonEmpty (InputG t r newSigData)))
signInputs tx@BtcTx{..}  = do
    resE <- zipWithM (signInput tx) [0..] (NE.toList btcIns)
    let errors = lefts (resE :: [Either SignKeyError (InputG t r newSigData)])
    return $ if null errors
        then Right $ unsafeCastNE (rights resE)
        else Left    errors

signInput
    :: forall m t r signKey oldSigData newSigData.
       ( TransformSigData newSigData oldSigData r
       , MonadSign m signKey
       , HasSigningKey signKey t r oldSigData
       )
    => BtcTx t r oldSigData
    -> Word32
    -> InputG t r oldSigData
    -> m (Either SignKeyError (InputG t r newSigData))
signInput tx idx inp@MkInputG{..} = do
         SignConf{..} <- getSignConf
         signKey <- signGetKey
         let prv = getSignKey inp (signKey :: signKey)
         let rawSig = getHashForSig tx btcCondScr idx btcSignFlag `HC.signMsg` prv
             newSigData :: Tagged r newSigData
             newSigData = mkSigData btcSigData (BtcSig rawSig btcSignFlag)
             signPK  = unTagged (signerPubKey btcCondScr :: Tagged newSigData PubKeyC)
             realPK  = HC.derivePubKey prv
             retVal  = Right $ mapSigData (const $ unTagged newSigData) inp
         return $ if realPK == signPK
            then retVal
            else if doSignCheck
                    then Left $ SignKeyError idx (realPK `FoundButExpected` signPK)
                    else retVal


-- ####################
-- ### Verification ###

verifyTx :: (SpendFulfillment ss r, SpendCondition r) =>
                BtcTx t r ss -> Either VerifyError ()
verifyTx tx@BtcTx{..} =
    if null verifyRes then Right () else Left $ SigVerifyFail $ map snd verifyRes
  where
    verifyRes = concatMap getErrors $ zipWith (verifyInput tx) [0..] (NE.toList btcIns)
    getErrors = filter ((== False) . fst)

-- TODO: fix SIG_SINGLE/SIG_NONE verify bug
verifyInput :: forall r t ss.
               (SpendFulfillment ss r, SpendCondition r) =>
                  BtcTx t r ss
               -> Word32
               -> InputG t r ss
               -> [(Bool, (Word32, PubKey, HC.Hash256, HC.Signature))]
verifyInput tx idx MkInputG{..} = do
         let getHash = getHashForSig tx btcCondScr idx
         let keySigL = rawSigs btcSigData btcCondScr
         let sigVerify (pk, BtcSig sig flag) =
                ( HC.verifySig (getHash flag) sig pk
                , (idx, pk, getHash flag, sig)
                )
         map sigVerify keySigL

getHashForSig ::
    SpendCondition r => BtcTx t r a -> r -> Word32 -> HS.SigHash -> HC.Hash256
getHashForSig tx rdmScr idx = HS.txSigHash
    (toUnsignedTx tx) (conditionScript rdmScr) (toInt idx)


txSize :: SignatureScript r ss t => BtcTx t r ss -> TxByteSize
txSize = calcTxSize . toHaskoinTx

mkRelativeFeeTxM
    :: (Monad m, HasFee fee, SignatureScript r ss t)
    => fee                                          -- ^ Desired transaction fee
    -> ( BtcAmount -> m (Either e (BtcTx t r ss)) )   -- ^ Produces desired Bitcoin tx with given fee
    -> m (Either e (BtcTx t r ss))
mkRelativeFeeTxM fee mkTxFunc =
    mkTxFunc (0 :: BtcAmount) >>= \txE ->
        case txE of
            Right tx -> mkTxSizeFee tx
            left     -> return left
    where
        mkTxSizeFee tx = mkTxFunc $ absoluteFee (fromIntegral $ availableVal tx) (txSize tx) fee


