{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bitcoin.PaymentChannel.Internal.Crypto.PubKey
(   IsPubKey(..)
,   SendPubKey(..)
,   RecvPubKey(..)
) where

import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize     as Bin

class Bin.Serialize a => IsPubKey a where
    getPubKey :: a -> HC.PubKeyC

newtype SendPubKey = MkSendPubKey {
    getSenderPK    :: HC.PubKeyC
} deriving (Eq, Show, Bin.Serialize)
instance IsPubKey SendPubKey where
    getPubKey = getSenderPK

newtype RecvPubKey = MkRecvPubKey {
    getReceiverPK  :: HC.PubKeyC
} deriving (Eq, Show, Bin.Serialize)
instance IsPubKey RecvPubKey where
    getPubKey = getReceiverPK

instance IsPubKey HC.XPubKey where
    getPubKey = HC.xPubKey
