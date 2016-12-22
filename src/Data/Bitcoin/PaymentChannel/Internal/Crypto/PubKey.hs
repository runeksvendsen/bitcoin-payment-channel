{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bitcoin.PaymentChannel.Internal.Crypto.PubKey
(   IsPubKey(..)
,   SendPubKey(..)
,   RecvPubKey(..)
,   HasSendPubKey(..)
,   HasRecvPubKey(..)
,   KeyDeriveIndex
) where

import           Data.Bitcoin.PaymentChannel.Internal.Util
import qualified Network.Haskoin.Crypto as HC
import           Data.Word              (Word32)


-- |Key index for a BIP32 root key
newtype KeyDeriveIndex = KeyDeriveIndex Word32
    deriving (Eq, Show, Serialize, Ord, Num, Enum, Real, Integral, FromJSON, ToJSON)

-- |Types which contain a pubkey
class Serialize a => IsPubKey a where
    getPubKey :: a -> HC.PubKeyC

-- |Wrapper for value sender's public key
newtype SendPubKey = MkSendPubKey {
    getSenderPK    :: HC.PubKeyC
} deriving (Eq, Show, Serialize, FromJSON, ToJSON)
instance IsPubKey SendPubKey where
    getPubKey = getSenderPK

-- |Wrapper for value receiver's public key
newtype RecvPubKey = MkRecvPubKey {
    getReceiverPK  :: HC.PubKeyC
} deriving (Eq, Show, Serialize, FromJSON, ToJSON)
instance IsPubKey RecvPubKey where
    getPubKey = getReceiverPK

instance IsPubKey HC.XPubKey where
    getPubKey = HC.xPubKey

-- |Types which contain a 'SendPubKey'
class HasSendPubKey a where
    getSendPubKey :: a -> SendPubKey

class HasRecvPubKey a where
    getRecvPubKey :: a -> RecvPubKey

