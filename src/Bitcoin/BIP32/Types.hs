{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Bitcoin.BIP32.Types
( -- * Key types
  RootPrv
, RootPub
, RootKeyId
, External
, Internal
, ChildPair
, ChildPub
, ExtPub
, ExtPrv
, createRootPrv
, fromRootPrv
  -- * Child key operations
, IsChildKey(..)
, HasKey(..)
, KeyDeriveIndex
, HasKeyIndex(..)
  -- * Util
, keyId
, fromExternalPair
  -- * Re-exports
, HC.SoftPath
, HC.HardPath
)
where


import Bitcoin.Internal.Orphans               ()
import Bitcoin.Internal.Util
import qualified Network.Haskoin.Crypto       as HC
import qualified Network.Haskoin.Node         as HN
import           Network.Haskoin.Crypto       (DerivPathI(..))
import qualified Data.ByteString.Char8        as C8
import qualified Data.Serialize               as Bin


createRootPrv :: ByteString -> RootPrv
createRootPrv = RootPrv . HC.makeXPrvKey

-- | Source of all other key types
newtype RootPrv = RootPrv HC.XPrvKey deriving (Generic, NFData, Serialize, FromJSON, ToJSON)

-- | The public counterpart of a 'RootPrv'
newtype RootPub = RootPub HC.XPubKey deriving (Eq, Show, Generic, NFData, Serialize, FromJSON, ToJSON)

-- | Unique ID for a 'RootPrv'/'RootPub' pair
newtype RootKeyId = RootKeyId HC.Hash256 deriving (Eq, Show, Generic, NFData, Serialize, FromJSON, ToJSON)

class HasKeyId rk where
    keyId :: rk -> RootKeyId

instance HasKeyId RootPub where
    keyId (RootPub xPub) =
        RootKeyId . HC.hash256 . Bin.encode $ xPub

instance HasKeyId RootPrv where
    keyId = keyId . fromRootPrv

fromRootPrv :: RootPrv -> RootPub
fromRootPrv (RootPrv xPrv) =
    RootPub . HC.deriveXPubKey $ xPrv

-- | A child key derived from a 'RootPrv' or 'RootPub'
data ChildKey a = ChildKey a RootKeyId
    deriving (Eq, Show, Functor, Generic)

-- | A key pair where the public part can be derived without knowledge of the private part
--   (not safe if a single derived private key is exposed)
data External a = External (ChildKey a) HC.SoftPath
    deriving (Eq, Show, Functor, Generic)

-- | A key pair where the public part cannot be derived without knowledge of the private part
--   (safe if a single derived private key is exposed)
data Internal a = Internal (ChildKey a) HC.HardPath
    deriving (Eq, Show, Functor, Generic)

-- | Key pair, containing both private and public extended keys.
--   Derive subkeys using 'getKey'
data    ChildPair = ChildPair { pairPriv  :: !HC.XPrvKey
                              , pairPub'  :: !HC.XPubKey } deriving (Eq, Show, Generic)

-- | Public part only of a 'ChildPair'
newtype ChildPub  = ChildPub HC.XPubKey
    deriving (Eq, Show, Generic, NFData, Serialize, FromJSON, ToJSON)

type ExtPub = External ChildPub
type ExtPrv = External ChildPair

-- |
class IsChildKey sourceKey (t :: * -> *) k derivPath | t -> derivPath where
    -- Create top-level child key
    mkChild :: sourceKey -> derivPath -> t k

instance IsChildKey RootPrv Internal ChildPair HC.HardPath where
    mkChild rk@(RootPrv k) path = Internal
        (childKeyWrap rk . fromChildPrv . HC.derivePath path $ k) path


instance IsChildKey RootPrv External ChildPair HC.SoftPath where
    mkChild rk@(RootPrv k) path = External
        (childKeyWrap rk . fromChildPrv . HC.derivePath path $ k) path


instance IsChildKey RootPrv External ChildPub HC.SoftPath where
    mkChild k = fmap fromPair . mkChild k

instance IsChildKey RootPub External ChildPub HC.SoftPath where
    mkChild rk@(RootPub k) path = External hey path
      where hey = childKeyWrap rk . ChildPub . HC.derivePubPath path $ k


fromChildPrv :: HC.XPrvKey -> ChildPair
fromChildPrv prv = ChildPair prv (HC.deriveXPubKey prv)

fromPair :: ChildPair -> ChildPub
fromPair = ChildPub . pairPub'

fromExternalPair :: External ChildPair -> External ChildPub
fromExternalPair = fmap fromPair


class HasKey t k key derivPath | t -> derivPath where
    getKey  :: t k -> key

instance HasKey Internal ChildPair (HC.PrvKeyC, HC.XPubKey) HC.HardPath where
    getKey (Internal (ChildKey (ChildPair k _) _) _) =
        (HC.xPrvKey k,  HC.deriveXPubKey k)

instance HasKey Internal ChildPair HC.PrvKeyC HC.HardPath where
    getKey pair = fst (getKey pair :: (HC.PrvKeyC, HC.XPubKey))

instance HasKey Internal ChildPair HC.XPubKey HC.HardPath where
    getKey pair = snd (getKey pair :: (HC.PrvKeyC, HC.XPubKey))

instance HasKey Internal ChildPair HC.PubKeyC HC.HardPath where
    getKey pair = HC.xPubKey $ getKey pair

instance HasKey External ChildPair (HC.PrvKeyC, HC.XPubKey) HC.SoftPath where
    getKey (External (ChildKey (ChildPair k _) _) _) =
        (HC.xPrvKey k,  HC.deriveXPubKey k)

instance HasKey External ChildPair HC.PrvKeyC HC.SoftPath where
    getKey pair = fst (getKey pair :: (HC.PrvKeyC, HC.XPubKey))

instance HasKey External ChildPair HC.XPubKey HC.SoftPath where
    getKey pair = snd (getKey pair :: (HC.PrvKeyC, HC.XPubKey))

instance HasKey External ChildPair HC.PubKeyC HC.SoftPath where
    getKey pair = HC.xPubKey $ getKey pair

instance HasKey External ChildPub HC.XPubKey HC.SoftPath where
    getKey (External (ChildKey (ChildPub pk) _) _) = pk

instance HasKey External ChildPub HC.PubKeyC HC.SoftPath where
    getKey = HC.xPubKey . getKey


-- ##########
-- ### Serialization

instance Serialize a => Serialize (ChildKey a) where
    get = ChildKey <$> get <*> get
    put (ChildKey key kId) = put key >> put kId

instance Serialize a => Serialize (External a) where
    get = External <$> get <*> desSoftPath
    put (External a p) = put a >> serPath p

instance Serialize a => Serialize (Internal a) where
    get = Internal <$> get <*> desHardPath
    put (Internal a p) = put a >> serPath p

serPath :: DerivPathI t -> PutM ()
serPath sp = Bin.put (HN.VarInt . fromIntegral . C8.length $ strBuf)
              >> Bin.putByteString strBuf
  where strBuf = C8.pack $ HC.pathToStr sp

desSoftPath :: Get HC.SoftPath
desSoftPath = do
    strBuf <- varIntString
    maybe (fail $ "failed to parse SoftPath from string: " ++ show strBuf)
          return
          (HC.parseSoft strBuf)

desHardPath :: Get HC.HardPath
desHardPath = do
    strBuf <- varIntString
    maybe (fail $ "failed to parse HardPath from string: " ++ show strBuf)
          return
          (HC.parseHard strBuf)

-- | Get 'HN.VarInt' length-prefixed 'String'
varIntString :: Get String
varIntString = do
    HN.VarInt len <- Bin.get
    C8.unpack <$> Bin.getByteString (fromIntegral len)



-- |Key index for a BIP32 child key
newtype KeyDeriveIndex = KeyDeriveIndex Word32
    deriving (Eq, Show, Serialize, Ord, Num, Enum, Real, Integral, FromJSON, ToJSON, NFData)

class HasKeyIndex a where
    getKeyIndex :: a -> KeyDeriveIndex

instance HasKeyIndex HC.XPubKey where
    getKeyIndex = KeyDeriveIndex . HC.xPubIndex


-- ###########
-- ##  UTIL

childKeyWrap :: HasKeyId rk => rk -> a -> ChildKey a
childKeyWrap rk a = ChildKey a (keyId rk)

{-
word32Index :: KeyDeriveIndex -> Word32
word32Index (KeyDeriveIndex i) = i

-- | Ignore most significant bit
word31Index :: KeyDeriveIndex -> Word32
word31Index (KeyDeriveIndex i) = i `mod` (2^31 :: Word32)

mkKeyIndex :: Word32 -> Maybe KeyDeriveIndex
mkKeyIndex i
    | i >= 0 && i < 0x80000000 = Just $ KeyDeriveIndex i
    | otherwise = Nothing
-}

instance FromJSON a => FromJSON (External a)
instance FromJSON a => FromJSON (ChildKey a)
instance ToJSON a => ToJSON (External a)
instance ToJSON a => ToJSON (ChildKey a)
instance NFData ChildPair
instance NFData a => NFData (External a)
instance NFData a => NFData (Internal a)
instance NFData a => NFData (ChildKey a)