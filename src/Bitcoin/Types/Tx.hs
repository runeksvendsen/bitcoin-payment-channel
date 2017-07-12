{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures, DeriveAnyClass, DeriveFunctor #-}
module Bitcoin.Types.Tx
( module Bitcoin.Types.Tx
, module X
, Default(..)
)

where

import Bitcoin.Internal.Orphans               as X ()
import Bitcoin.Dust                           as X
import Bitcoin.Amount                         as X
import Bitcoin.Fee                            as X
import Bitcoin.Error                          as X
import Bitcoin.LockTime.Types                 as X
--import Bitcoin.BIP32
--import Bitcoin.Util
import Bitcoin.Internal.Util

import qualified Data.List.NonEmpty     as NE
import qualified Data.Serialize         as Bin
--import qualified Data.Serialize.Get     as BinGet
import qualified Data.Aeson.Types       as JSON
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC
import           Data.Word              (Word32)
import           Control.DeepSeq        (NFData)
import           Data.Default.Class     (Default(..))


data BtcTx inType condScr sigData = BtcTx
    { btcVer    :: Word32
    , btcIns    :: NE.NonEmpty (InputG inType condScr sigData)
    , btcOuts   :: [BtcOut]
    , btcChgOut :: Maybe ChangeOut
    , btcLock   :: Maybe LockTimeDate
    } deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, NFData)

-- | Generic input
data InputG inType condScr sigData =
    MkInputG
    { btcPrevOut      :: HT.OutPoint
    -- ^ The output we're redeeming
    , btcInValue      :: BtcAmount
    -- ^ Value of output that is redeemed
    , btcSigData      :: sigData
    -- ^ The type of the signature data, eg. '()' for unsigned
    , btcCondScr      :: condScr
    -- ^ ConditionScript type; represents payment channel contract.
    --    'inType' determines where this is located in the Bitcoin transaction
    --    (redeeming input or paying output)
    , btcSequence     :: Word32
    -- ^ Input sequence (non-default only needed to enable locktime features)
    , btcSignFlag     :: HS.SigHash
    -- ^ SigHash flag used to sign this input (default: SIGHASH_ALL)
{-    , btcKeyIndex     :: KeyDeriveIndex
    -- ^ BIP32 key index for key used to sign this input (optional)
-}
    } deriving (Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON, NFData)

-- | Generic output
data OutputG outType condScr =
    MkOutputG
    { btcOutAmount  :: BtcAmount
    , btcOutCond    :: condScr
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON, NFData)

data BtcOut = BtcOut
    { btcAddress    :: HC.Address
    , btcAmount     :: NonDustyAmount
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON, NFData)

-- | An input/output pair signed with the SigHash flag SIG_SINGLE|ANYONECANPAY,
--    meaning that only a single input/output pair is signed (SIG_SINGLE) --
--    rather than all tx inputs/outputs -- and that additional inputs can be added
--    later to the tx (ANYONECANPAY).
--   As a special case, if the output amount equals zero, the input is signed
--    with SIG_NONE|ANYONECANPAY and the output removed, resulting in all value
--    from the funding output being transferred to the server.
data SigSinglePair t r sd = SigSinglePair
    { singleInput   :: InputG t r sd
    , singleOutput  :: BtcOut
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON, NFData)

instance (Eq condScr, Eq sigData) => Eq (InputG inType condScr sigData) where
    (MkInputG prevOut1 inVal1 sigData1 condScr1 seq1 _) ==
        (MkInputG prevOut2 inVal2 sigData2 condScr2 seq2 _) =
            prevOut1 == prevOut2
              && inVal1 == inVal2
              && sigData1 == sigData2
              && condScr1 == condScr2
              && seq1 == seq2
              -- Ignore sign flag

instance Eq r => Ord (SigSinglePair t r BtcSig) where
    compare a b = compare (sigFlag a) (sigFlag b)
        where sigFlag = bsSigFlag . btcSigData . singleInput

data ChangeOut = ChangeOut
    { btcChangeAddr :: HC.Address
    , btcTxFee      :: TxFee
    , btcDustPolicy :: DustPolicy
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON, NFData)


data TxFee
  = AbsoluteFee BtcAmount
  | RelativeFee SatoshisPerByte
  | MaximumFee (MaxFee BtcAmount SatoshisPerByte)
      deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON, NFData)

toMaxFee
    :: TxFee
    -> MaxFee BtcAmount SatoshisPerByte
toMaxFee (AbsoluteFee val) = MaxFee (val, 0  )
toMaxFee (RelativeFee spb) = MaxFee (0  , spb)
toMaxFee (MaximumFee maxFee) = maxFee

data DustPolicy = KeepDust | DropDust
    deriving (Eq, Show, Typeable, Generic, Bin.Serialize, JSON.ToJSON, JSON.FromJSON, NFData)

instance Default DustPolicy where def = DropDust

type UnsignedBtcTx t r = BtcTx t r ()
type UnsignedBtcIn t r = InputG t r ()

-- | ECDSA signature plus sig hash flag
data BtcSig = BtcSig
    { bsSig     ::  HC.Signature
    , bsSigFlag ::  HS.SigHash
    } deriving (Eq, Show, Typeable, Generic, Bin.Serialize, NFData)

instance ToJSON BtcSig where
    toJSON = object . paySigKV
        where paySigKV (BtcSig sig flag) =
                [ "signature_data"  .= String (serHex sig)
                , "sighash_flag"    .= String (serHex flag) ]
instance FromJSON BtcSig where
    parseJSON = withObject "BtcSig" $ \o ->
        BtcSig <$>
            (o .: "signature_data" >>= withText "SigDataHex"  deserHex) <*>
            (o .: "sighash_flag"   >>= withText "HashFlagHex" deserHex)

newtype InvalidSig = MkInvalidSig HS.SigHash
    deriving (Eq, Show, Typeable, Generic, Bin.Serialize, NFData)

fromBtcSig :: BtcSig -> InvalidSig
fromBtcSig = MkInvalidSig . bsSigFlag


-- -- Ignore signFlags and keyIndex (metadata)
--instance (Eq r, Eq sd) => Eq (InputG typ r sd) where
--    (MkInputG po1 inv1 sd1 r1 seq1) == (MkInputG po2 inv2 sd2 r2 seq2) =
--        po1 == po2 && inv1 == inv2 && sd1 == sd2 && r1 == r2 && seq1 == seq2

instance Eq (IgnoreSigData BtcSig) where
    IgnoreSigData (BtcSig _ flag1) == IgnoreSigData (BtcSig _ flag2) =
            flag1 == flag2

newtype AlwaysEq a = AlwaysEq a
instance Eq (AlwaysEq a) where _ == _ = True
newtype IgnoreSigData a = IgnoreSigData a deriving (Show, Functor)


instance Eq rdmScr => Eq (IgnoreSigData (BtcTx inType rdmScr BtcSig)) where
    IgnoreSigData tx1 == IgnoreSigData tx2 =
            mapSigData IgnoreSigData tx1 == mapSigData IgnoreSigData tx2


-- | Types that can be converted to/from a 'BtcTx',
--    parametized over redeemScript and signature data type.
class IsTxLike (txLike :: * -> * -> * -> *) t r sd where
    toBtcTx    :: txLike t r sd -> BtcTx t r sd
    fromBtcTx  :: BtcTx t r sd -> txLike t r sd

instance IsTxLike BtcTx t r ss where
    toBtcTx   = id
    fromBtcTx = id

instance (Show r, Show sd) => IsTxLike SigSinglePair t r sd where
    toBtcTx SigSinglePair{..} =
        BtcTx 1 inputL [singleOutput] Nothing Nothing
            where inputL  = singleInput NE.:| []
    fromBtcTx tx@BtcTx{..}
        | btcVer == 1 && isNothing btcLock = SigSinglePair input output
        | otherwise = error $ "SigSinglePair: Modified transaction data: " ++ show tx
            where input  = head . NE.toList $ btcIns
                  output = head btcOuts

-- Defaults
defaultTxVersion :: Word32
defaultTxVersion = 1

defaultSigHashFlag :: HS.SigHash
defaultSigHashFlag = HS.SigAll False


-- Simple constructors
mkBtcTx :: NE.NonEmpty (InputG t r sd) -> [BtcOut] -> BtcTx t r sd
mkBtcTx ins outs = BtcTx defaultTxVersion ins outs Nothing Nothing

mkNoSigTxIn :: HT.OutPoint -> BtcAmount -> r -> UnsignedBtcIn t r
mkNoSigTxIn op val t = MkInputG op val () t maxBound defaultSigHashFlag

mkBtcOut :: HC.Address -> NonDustyAmount -> BtcOut
mkBtcOut = BtcOut

class HasFee fee => ToChangeOutFee fee where
    mkChangeFee :: fee -> TxFee

instance ToChangeOutFee SatoshisPerByte where
    mkChangeFee = RelativeFee

instance ToChangeOutFee BtcAmount where
    mkChangeFee = AbsoluteFee

txAddOuts :: [BtcOut] -> BtcTx t r sd -> BtcTx t r sd
txAddOuts outs tx = tx { btcOuts = btcOuts tx ++ outs }


-- Util
class HasSigData (t :: * -> *) where
    mapSigData :: (a -> b) -> t a -> t b

instance HasSigData (InputG t r) where
    mapSigData f bin = bin { btcSigData = f $ btcSigData bin }

--mapInputType :: (ta -> tb) -> InputG ta r a -> InputG tb r a
--mapInputType f bin = bin { btcInType = f $ btcInType bin }

instance HasSigData (BtcTx t r) where
    mapSigData f tx@BtcTx{..} =
        tx { btcIns = NE.map mapIn btcIns }
            where mapIn = mapSigData f

instance HasSigData (SigSinglePair t r) where
    mapSigData f sp@SigSinglePair{..} = sp { singleInput = mapSigData f singleInput }


setSequence :: Word32 -> InputG t r a -> InputG t r a
setSequence s bin = bin { btcSequence = s }

availableVal :: BtcTx t r sd -> Int64
availableVal BtcTx{..} =
    fromIntegral inVal - fromIntegral outVal
    where
        inVal  = sum . NE.toList $ NE.map btcInValue btcIns
        outVal = sum $ map (nonDusty . btcAmount) btcOuts


setSignFlag :: HS.SigHash -> InputG t r a -> InputG t r a
setSignFlag sh inp = inp { btcSignFlag = sh }

setLockTime :: LockTimeDate -> BtcTx t r sd -> BtcTx t r sd
setLockTime lt tx = tx { btcLock = Just lt }

setChangeOut :: ChangeOut -> BtcTx t r sd -> BtcTx t r sd
setChangeOut co tx = tx { btcChgOut = Just co }

disableLockTime :: BtcTx t r sd -> BtcTx t r sd
disableLockTime tx = tx { btcLock = Nothing }

setAbsFee :: BtcAmount -> ChangeOut -> ChangeOut
setAbsFee fee co = co { btcTxFee = AbsoluteFee fee }

setTxRawFee :: BtcAmount -> BtcTx t r sd -> BtcTx t r sd
setTxRawFee fee tx@BtcTx{..} = tx { btcChgOut = setAbsFee fee <$> btcChgOut }


-- Util
unsafeCastNE :: [a] -> NE.NonEmpty a
unsafeCastNE = fromMaybe (error "you promised this was a non-empty list") . NE.nonEmpty



-- Conversion

instance (Bin.Serialize r, Bin.Serialize sd) => Bin.Serialize (BtcTx t r sd) where
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
newtype TxOutputScript t = TxOutputScript [HS.ScriptOp] deriving Eq
newtype TxInputScript  t = TxInputScript  [HS.ScriptOp] deriving Eq
newtype WitnessScript  t = WitnessScript  [HS.ScriptOp] deriving Eq

class IsScript a where
    mkScript :: HS.Script -> a
    asScript :: a -> HS.Script

instance IsScript (TxOutputScript t) where
    mkScript (HS.Script ops) = TxOutputScript ops
    asScript (TxOutputScript ops) = HS.Script ops

instance IsScript (TxInputScript t) where
    mkScript (HS.Script ops) = TxInputScript ops
    asScript (TxInputScript ops) = HS.Script ops

instance IsScript (WitnessScript t) where
    mkScript (HS.Script ops) = WitnessScript ops
    asScript (WitnessScript ops) = HS.Script ops

instance Show (TxOutputScript t) where
    show (TxOutputScript ops) =
        "scriptPubKey: " ++ articulate ops
instance Show (TxInputScript t) where
    show (TxInputScript ops) =
        "scriptSig:    " ++ articulate ops
instance Show (WitnessScript t) where
    show (WitnessScript ops) =
        "witness:      " ++ articulate ops

articulate :: forall a. Show a => [a] -> String
articulate ops = if null ops then "(empty)" else unwords (map show ops)


type PubKey = HC.PubKeyC



