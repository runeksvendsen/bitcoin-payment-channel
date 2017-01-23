{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures, DeriveAnyClass, DeriveFunctor #-}
module Bitcoin.Types
(
  module Bitcoin.Types
, module X
)

where

import Bitcoin.Orphans  as X
import Bitcoin.Dust     as X
import Bitcoin.Amount   as X
import Bitcoin.Fee      as X
import Bitcoin.Util     as X
import Bitcoin.Error    as X
import PaymentChannel.Internal.Crypto.PubKey    as X
import Bitcoin.LockTime.Types    as X

import qualified Data.List.NonEmpty     as NE
import qualified Data.ByteString        as B
import qualified Data.Serialize         as Bin
import qualified Data.Aeson.Types       as JSON
import           Data.Word              (Word32)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC


data BtcTx inType sigData = BtcTx
    { btcVer    :: Word32
    , btcIns    :: NE.NonEmpty (InputG inType sigData)
    , btcOuts   :: [BtcOut]
    , btcChgOut :: Maybe ChangeOut
    , btcLock   :: Maybe LockTimeDate
    } deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON)


-- | Generic input
data InputG inType sigData =
    MkInputG
    { btcPrevOut    :: HT.OutPoint      -- ^ The output we're redeeming
    , btcInValue    :: BtcAmount        -- ^ Value of output that is redeemed
    , btcSigData    :: sigData          -- ^ The type of the signature data, eg. '()' for unsigned
    , btcInType     :: inType
    , btcSequence   :: Word32           -- ^ Input sequence (non-default only needed to enable locktime features)
    , btcSignFlag   :: HS.SigHash       -- ^ SigHash flag used to sign this input (default: SIGHASH_ALL)
    , btcKeyIndex   :: KeyDeriveIndex   -- ^ BIP32 key index for key used to sign this input (optional)
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON)

-- | Generic output
data OutputG outType =
    MkOutputG
    { btcOutAmount  :: BtcAmount
    , btcOutType    :: outType
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON)

data BtcOut = BtcOut
    { btcAddress    :: HC.Address
    , btcAmount     :: NonDusty BtcAmount
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON)

-- | An input/output pair signed with the SigHash flag SIG_SINGLE|ANYONECANPAY,
--    meaning that only a single input/output pair is signed (SIG_SINGLE) --
--    rather than all tx inputs/outputs -- and that additional inputs can be added
--    later to the tx (ANYONECANPAY).
--   As a special case, if the output amount equals zero, the input is signed
--    with SIG_NONE|ANYONECANPAY and the output removed, resulting in all value
--    from the funding output being transferred to the server.
data SigSinglePair t sd = SigSinglePair
    { singleInput   :: InputG t sd
    , singleOutput  :: BtcOut
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON)

instance Eq t => Ord (SigSinglePair t BtcSig) where
    compare a b = compare (sigFlag a) (sigFlag b)
        where sigFlag = bsSigFlag . btcSigData . singleInput

data ChangeOut = ChangeOut
    { btcChangeAddr :: HC.Address
    , btcTxFee      :: SatoshisPerByte
    , btcDustPolicy :: DustPolicy
      -- | For internal use.
    , btcAbsFee_    :: BtcAmount
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON)

data DustPolicy = KeepDust | DropDust
    deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON)

type UnsignedBtcTx t = BtcTx t ()
type UnsignedBtcIn t = InputG t ()

-- | ECDSA signature plus sig hash flag
data BtcSig = MkBtcSig
    { bsSig     ::  HC.Signature
    , bsSigFlag ::  HS.SigHash
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize)

instance ToJSON BtcSig where
    toJSON = object . paySigKV
        where paySigKV (MkBtcSig sig flag) =
                [ "signature_data"  .= String (serHex sig)
                , "sighash_flag"    .= String (serHex flag) ]
instance FromJSON BtcSig where
    parseJSON = withObject "BtcSig" $ \o ->
        MkBtcSig <$>
            (o .: "signature_data" >>= withText "SigDataHex"  deserHex) <*>
            (o .: "sighash_flag"   >>= withText "HashFlagHex" deserHex)





instance Eq (IgnoreSigData BtcSig) where
    IgnoreSigData (MkBtcSig _ flag1) == IgnoreSigData (MkBtcSig _ flag2) =
            flag1 == flag2

data AlwaysEq a = AlwaysEq a
instance Eq (AlwaysEq a) where _ == _ = True
data IgnoreSigData a = IgnoreSigData a deriving (Show, Functor)


instance Eq inType => Eq (IgnoreSigData (BtcTx inType BtcSig)) where
    IgnoreSigData tx1 == IgnoreSigData tx2 =
            txMapSigData IgnoreSigData tx1 == txMapSigData IgnoreSigData tx2


-- | Types that can be converted to/from a 'BtcTx',
--    parametized over redeemScript and signature data type.
class IsTxLike (txLike :: * -> * -> *) r sd where
    toBtcTx    :: txLike r sd -> BtcTx r sd
    fromBtcTx  :: BtcTx r sd -> txLike r sd

instance IsTxLike BtcTx r ss where
    toBtcTx   = id
    fromBtcTx = id

data VerifyError =
    SigVerifyFail [(Word32,HC.PubKeyC,HC.Hash256,HC.Signature)]
        deriving (Eq, Show, Typeable, Generic) -- , Bin.Serialize, JSON.ToJSON, JSON.FromJSON)

-- Defaults
defaultTxVersion :: Word32
defaultTxVersion = 1

defaultSigHashFlag :: HS.SigHash
defaultSigHashFlag = HS.SigAll False


-- Simple constructors
mkBtcTx :: NE.NonEmpty (InputG t sd) -> [BtcOut] -> BtcTx t sd
mkBtcTx ins outs = BtcTx defaultTxVersion ins outs Nothing Nothing

mkNoSigTxIn :: HT.OutPoint -> BtcAmount -> r -> UnsignedBtcIn r
mkNoSigTxIn op val t = MkInputG op val () t maxBound defaultSigHashFlag 0

mkBtcOut :: HC.Address -> NonDusty BtcAmount -> BtcOut
mkBtcOut = BtcOut

mkChangeOut :: HC.Address -> SatoshisPerByte -> DustPolicy -> ChangeOut
mkChangeOut chgAdr fee dp = ChangeOut chgAdr fee dp 0

txAddOuts :: [BtcOut] -> BtcTx r a -> BtcTx r a
txAddOuts outs tx = tx { btcOuts = btcOuts tx ++ outs }

-- Util
mapSigData :: (a -> b) -> InputG t a -> InputG t b
mapSigData f bin = bin { btcSigData = f $ btcSigData bin }

txMapSigData :: (a -> b) -> BtcTx t a -> BtcTx t b
txMapSigData f tx@BtcTx{..} =
    tx { btcIns = NE.map mapIn btcIns }
        where mapIn = mapSigData f

setSequence :: Word32 -> InputG t a -> InputG t a
setSequence s bin = bin { btcSequence = s }

availableVal :: BtcTx r a -> Int64
availableVal BtcTx{..} =
    fromIntegral inVal - fromIntegral outVal
    where
        inVal  = sum . NE.toList $ NE.map btcInValue btcIns
        outVal = sum $ map (nonDusty . btcAmount) btcOuts



setSignFlag :: HS.SigHash -> InputG t a -> InputG t a
setSignFlag sh inp = inp { btcSignFlag = sh }

setLockTime :: LockTimeDate -> BtcTx r a -> BtcTx r a
setLockTime lt tx = tx { btcLock = Just lt }

disableLockTime :: BtcTx r a -> BtcTx r a
disableLockTime tx = tx { btcLock = Nothing }

setKeyIndex :: KeyDeriveIndex -> InputG t a -> InputG t a
setKeyIndex kdi bin = bin { btcKeyIndex = kdi }

setFee :: BtcAmount -> ChangeOut -> ChangeOut
setFee fee co = co { btcAbsFee_ = fee }

setTxRawFee :: BtcAmount -> BtcTx r a -> BtcTx r a
setTxRawFee fee tx@BtcTx{..} = tx { btcChgOut = setFee fee <$> btcChgOut }


-- Util
unsafeCastNE :: [a] -> NE.NonEmpty a
unsafeCastNE = fromMaybe (error "you promised this was a non-empty list") . NE.nonEmpty




-- Conversion

-- | Create inputs from P2SH outputs that pay to the given redeemScript
-- getPrevIn :: SpendCondition r => HT.Tx -> Pay2 (ScriptHash (Cond r)) -> [InputG r ()]
-- getPrevIn tx rdmScr =
--     map mkInput $ catMaybes $
--         zipWith relevantIndexes [0..] (HT.txOut tx)
--   where
--     scrHash = scriptHash160 $ conditionScript rdmScr
--     mkInput (idx,val) = mkNoSigTxIn (mkPrevOut idx) (fromIntegral val) rdmScr
--     mkPrevOut = HT.OutPoint (HT.txHash tx)
--     checkMatch idx (hash,val) = if hash == scrHash then Just (idx,val) else Nothing
--     relevantIndexes idx out =
--         either (const Nothing) (checkMatch idx)
--             (decodeScriptHash (HT.scriptOutput out) >>= \sh -> Right (sh, HT.outValue out))

instance (Bin.Serialize t, Bin.Serialize sd) => Bin.Serialize (BtcTx t sd) where
    put BtcTx{..} =
           put btcVer
        >> put (NE.toList btcIns)
        >> put btcOuts
        >> put btcChgOut
        >> put btcLock
    get = BtcTx
        <$> get
        <*> fmap unsafeCastNE get
        <*> get
        <*> get
        <*> get


-- Script types
newtype TxOutputScript = TxOutputScript [HS.ScriptOp] deriving Eq
newtype TxInputScript  = TxInputScript  [HS.ScriptOp] deriving Eq
newtype WitnessScript  = WitnessScript  [HS.ScriptOp] deriving Eq
class IsScript a where
    mkScript :: HS.Script -> a
    asScript :: a -> HS.Script
instance IsScript TxOutputScript where
    mkScript (HS.Script ops) = TxOutputScript ops
    asScript (TxOutputScript ops) = HS.Script ops
instance IsScript TxInputScript  where
    mkScript (HS.Script ops) = TxInputScript ops
    asScript (TxInputScript ops) = HS.Script ops
instance IsScript WitnessScript  where
    mkScript (HS.Script ops) = WitnessScript ops
    asScript (WitnessScript ops) = HS.Script ops
instance Show TxOutputScript where
    show (TxOutputScript ops) =
        "scriptPubKey: " ++ articulate ops
instance Show TxInputScript where
    show (TxInputScript ops) =
        "scriptSig:    " ++ articulate ops
instance Show WitnessScript where
    show (WitnessScript ops) =
        "witness:      " ++ articulate ops

articulate :: forall a. Show a => [a] -> String
articulate ops = if null ops then "(empty)" else unwords (map show ops)


type PubKey = HC.PubKeyC


hashSigData :: BtcSig -> HC.Hash256
hashSigData MkBtcSig{..} = HC.hash256 (Bin.encode bsSig)
