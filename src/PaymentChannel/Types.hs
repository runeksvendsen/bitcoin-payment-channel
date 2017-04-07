{-|
Module      : PaymentChannel.Types
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com

Types used with the interface provided by "PaymentChannel".

-}

{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

module PaymentChannel.Types
(
    -- *Shared sender/receiver types/functions
    FundingTxInfo(..)
  , ChanParams(..)
  , PaymentChannel(..), PayChan
  , PaymentChannelRecv(..)
  , SharedSecret, HasSharedSecret(..), fromHash, toHash
  , fundingAddress
  , clientChangeAddress
  , availableChannelVal
  , getFundingAmount

    -- *Sender state
  , ClientPayChanI(..)

    -- *Receiver state
  , ServerPayChan, ServerPayChanI(rpcMetadata)
  , PayChanStatus(..), MetadataI(..), OpenError(..)
  , S.getChannelStatus, S.setChannelStatus
  , S.markAsBusy, S.isReadyForPayment

    -- *Receiver state (with pubkey metadata)
  , ServerPayChanX
  , S.mkExtendedKeyRPC, S.metaKeyIndex
  , KeyDeriveIndex
  , mkKeyIndex, word32Index

    -- *Payment
  , SignedPayment

    -- **Error
  , PayChanError(..)

    -- *Bitcoin
  , module X
  , module Bitcoin.Fee

    -- *Crypto
  , SendPubKey(..),RecvPubKey(..),IsPubKey(..),HasSendPubKey(..),HasRecvPubKey(..)

    -- *Util
  , module Bitcoin.SpendCond.Util
  , fromDate
  , getChanState
  , clientChangeVal
--  , usesBlockHeight, dummyPayment

    -- *Config settings
  , getSettlePeriod, getDustLimit

)
where

import PaymentChannel.Internal.Types
import PaymentChannel.Internal.Receiver.Types
import PaymentChannel.Internal.Metadata.Util
import PaymentChannel.Internal.Serialization ()
import PaymentChannel.Internal.Class.Value     (HasValue(..))
import PaymentChannel.Internal.Receiver.Open   (OpenError(..))
import Bitcoin.SpendCond.Util
import Bitcoin.Types as X

import qualified PaymentChannel.Internal.Receiver.Util as S
import qualified PaymentChannel.Internal.ChanScript as Script
import PaymentChannel.Internal.Error (PayChanError(..))
import Bitcoin.Fee

import qualified  Network.Haskoin.Crypto as HC


-- |Both sender and receiver state objects have a 'PaymentChannelState'
class HasPayChanState a where
    getPayChanState :: a -> PayChanState BtcSig

getFundingAmount  :: HasPayChanState a => a -> BtcAmount
getFundingAmount = fundingValue . pcsPayment . getPayChanState

fundingAddress :: HasPayChanState a => a -> HC.Address
fundingAddress = Script.getP2SHFundingAddress . pairRedeemScript . pcsPayment . getPayChanState

clientChangeAddress :: HasPayChanState a => a -> HC.Address
clientChangeAddress = clientChangeAddr . pcsPayment . getPayChanState

-- | Channel value left to send (subtracts dust limit from total available amount)
availableChannelVal :: HasPayChanState a => a -> BtcAmount
availableChannelVal a = clientChangeVal (pcsPayment $ getPayChanState a) - configDustLimit

-- | Legacy
getChanState :: HasPayChanState a => a -> PayChanState BtcSig
getChanState = getPayChanState

instance HasPayChanState ClientPayChan where
    getPayChanState = spcState

instance HasPayChanState (ServerPayChanI s) where
    getPayChanState = rpcState

-- |Get various information about an open payment channel.
class HasPayChanState a => PaymentChannel a where
    -- |Get amount received by receiver/left for sender
    valueToMe :: a -> BtcAmount
    -- |For internal use
    _setChannelState :: a -> PayChanState BtcSig -> a

-- clientChangeVal

instance PaymentChannel ClientPayChan where
    valueToMe = clientChangeVal . pcsPayment . spcState
    _setChannelState spc s = spc { spcState = s }

instance PaymentChannel (ServerPayChanI s) where
    valueToMe (MkServerPayChan s _) = valueOf s
    _setChannelState rpc s = rpc { rpcState = s }


-- |Payment channel state objects with metadata information
class PaymentChannel a => PaymentChannelRecv a where
    -- |Get total amount (both settled and unsettled) sent by client
    clientTotalValueSent :: a -> BtcAmount

instance PaymentChannelRecv (ServerPayChanI (MetadataI a)) where
    clientTotalValueSent = metaTotalValXfer . rpcMetadata


class HasSharedSecret a where
    getSecret :: a -> SharedSecret

instance HasSharedSecret (PayChanState a)   where getSecret = pcsSecret
instance HasSharedSecret (ServerPayChanI s) where getSecret = getSecret . rpcState
instance HasSharedSecret ClientPayChan      where getSecret = getSecret . spcState

-- |Short-hand
class PaymentChannel a => PayChan a

