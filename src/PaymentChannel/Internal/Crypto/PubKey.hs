{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module PaymentChannel.Internal.Crypto.PubKey
(   IsPubKey(..)
,   SendPubKey(..)
,   RecvPubKey(..)
,   HasSendPubKey(..)
,   HasRecvPubKey(..)
,   KeyDeriveIndex
,   HasKeyIndex(..)
) where

import           PaymentChannel.Internal.Util
import qualified Network.Haskoin.Crypto as HC
import           Data.Word              (Word32)


-- |Types which contain a pubkey
class Serialize a => IsPubKey a where
    getPubKey :: a -> HC.PubKeyC

instance IsPubKey HC.PubKeyC where
    getPubKey = id

-- |Wrapper for value sender's public key
newtype SendPubKey = MkSendPubKey {
    getSenderPK    :: HC.PubKeyC
} deriving (Eq, Show, Serialize, Generic, FromJSON, ToJSON, NFData)
instance IsPubKey SendPubKey where
    getPubKey = getSenderPK

-- |Wrapper for value receiver's public key
newtype RecvPubKey = MkRecvPubKey {
    getReceiverPK  :: HC.PubKeyC
} deriving (Eq, Show, Serialize, Generic, FromJSON, ToJSON, NFData)
instance IsPubKey RecvPubKey where
    getPubKey = getReceiverPK

instance IsPubKey HC.XPubKey where
    getPubKey = HC.xPubKey

-- |Types which contain a 'SendPubKey'
class HasSendPubKey a where
    getSendPubKey :: a -> SendPubKey

class HasRecvPubKey a where
    getRecvPubKey :: a -> RecvPubKey

-- |Key index for a BIP32 root key
newtype KeyDeriveIndex = KeyDeriveIndex Word32
    deriving (Eq, Show, Serialize, Ord, Num, Enum, Real, Integral, FromJSON, ToJSON, NFData)

class HasKeyIndex a where
    getKeyIndex :: a -> KeyDeriveIndex

instance HasKeyIndex HC.XPubKey where
    getKeyIndex = KeyDeriveIndex . HC.xPubIndex

-- instance HasKeyIndex RecvPubKey where
--     getKeyIndex = getKeyIndex . getReceiverPK

