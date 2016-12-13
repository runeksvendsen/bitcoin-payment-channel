{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bitcoin.PaymentChannel.Internal.Crypto.PubKey
(   IsPubKey(..)
,   SendPubKey(..)
,   RecvPubKey(..)
,   HasSendPubKey(..)
,   HasRecvPubKey(..)
) where

import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize     as Bin


-- |Types which contain a pubkey
class Bin.Serialize a => IsPubKey a where
    getPubKey :: a -> HC.PubKeyC

-- |Wrapper for value sender's public key
newtype SendPubKey = MkSendPubKey {
    getSenderPK    :: HC.PubKeyC
} deriving (Eq, Show, Bin.Serialize)
instance IsPubKey SendPubKey where
    getPubKey = getSenderPK

-- |Wrapper for value receiver's public key
newtype RecvPubKey = MkRecvPubKey {
    getReceiverPK  :: HC.PubKeyC
} deriving (Eq, Show, Bin.Serialize)
instance IsPubKey RecvPubKey where
    getPubKey = getReceiverPK

instance IsPubKey HC.XPubKey where
    getPubKey = HC.xPubKey

-- |Types which contain a 'SendPubKey'
class HasSendPubKey a where
    getSendPubKey :: a -> SendPubKey

class HasRecvPubKey a where
    getRecvPubKey :: a -> RecvPubKey
