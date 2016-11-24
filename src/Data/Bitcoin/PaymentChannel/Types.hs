{-|
Module      : Data.Bitcoin.PaymentChannel.Types
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com

Types used with the interface provided by "Data.Bitcoin.PaymentChannel".

-}

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Data.Bitcoin.PaymentChannel.Types
(
    -- *Interface
    PaymentChannel(..), PayChan,

    -- *State
    SenderPaymentChannel(..), SendPayChan,
    ReceiverPaymentChannel, ReceiverPaymentChannelI(rpcMetadata), RecvPayChan,
    ReceiverPaymentChannelX, RecvPayChanX,

    -- *Config
    Config(..),defaultConfig,
    FundingTxInfo(..),
    ChannelParameters(..), ChanParams,

    -- *Payment
    Payment,paySignature,
    FullPayment(..),
    -- **Error
    PayChanError(..)

    -- *Bitcoin
,   BitcoinAmount
,   BitcoinLockTime(..)
,   module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Fee

    -- *Crypto
,   SendPubKey(..),RecvPubKey(..),IsPubKey(..),HasSendPubKey(..),

    -- *Util
    S.mkExtendedKeyRPC, fromDate, usesBlockHeight

)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Serialization ()

import qualified Data.Bitcoin.PaymentChannel.Internal.State as S
import qualified Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script as Script
import Data.Bitcoin.PaymentChannel.Internal.Error (PayChanError(..))
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Fee

import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Transaction as HT
import            Data.Word (Word64)


-- |Get various information about an open payment channel.
class PaymentChannel a where
    -- |Get amount received by receiver/left for sender
    valueToMe           :: a -> BitcoinAmount
    -- |Retrieve internal state object
    getChannelState     :: a -> PaymentChannelState
    getChannelFunding   :: a -> HT.OutPoint
    getExpirationDate   :: a -> BitcoinLockTime
    getSenderPubKey     :: a -> SendPubKey
    getReceiverPubKey   :: a -> RecvPubKey
    getFundingAmount    :: a -> BitcoinAmount
    getPaymentCount     :: a -> Word64
    fundingAddress      :: a -> HC.Address
    getNewestPayment    :: a -> Payment
    getNewestSig        :: a -> HC.Signature
    -- |Return True if channel expires earlier than given expiration date
    expiresBefore       :: BitcoinLockTime -> a -> Bool
    -- |For internal use
    _setChannelState    :: a -> PaymentChannelState -> a

    channelValueLeft    :: a -> BitcoinAmount
    senderChangeValue   :: a -> BitcoinAmount
    -- |Returns 'True' if all available channel value has been transferred, 'False' otherwise
    channelIsExhausted  :: a -> Bool

    getChannelFunding       = S.pcsChannelFundingSource . getChannelState
    getExpirationDate  = S.pcsExpirationDate . getChannelState
    getSenderPubKey    = S.pcsClientPubKey . getChannelState
    getReceiverPubKey  = S.pcsServerPubKey . getChannelState
    getFundingAmount   = S.pcsChannelTotalValue . getChannelState
    getPaymentCount    = pcsPaymentCount . getChannelState
    fundingAddress  = Script.getP2SHFundingAddress . pcsParameters . getChannelState
    senderChangeValue  = pcsClientChangeVal . getChannelState
    channelValueLeft   = S.channelValueLeft . getChannelState
    channelIsExhausted = S.channelIsExhausted . getChannelState
    expiresBefore expDate chan = getExpirationDate chan < expDate
    getNewestPayment pcs = S.pcsGetPayment (getChannelState pcs)
    getNewestSig = psSig . paySignature . getNewestPayment

-- |State object for the value sender
data SenderPaymentChannel = CSenderPaymentChannel {
    -- |Internal state object
    spcState     ::  PaymentChannelState,
    -- |Payment-signing function
    spcSignFunc  ::  HC.Hash256 -> HC.Signature
}

instance PaymentChannel SenderPaymentChannel where
    valueToMe = channelValueLeft
    getChannelState = spcState
    _setChannelState spc s = spc { spcState = s }

instance PaymentChannel (ReceiverPaymentChannelI s) where
    valueToMe rpc@(CReceiverPaymentChannel s _) =
        S.pcsValueTransferred s
    getChannelState = rpcState
    _setChannelState rpc s = rpc { rpcState = s }

instance Show SenderPaymentChannel where
    show (CSenderPaymentChannel s _) =
        "<SenderPaymentChannel:\n\t" ++ show s ++ ">"


-- |Short-hand
type SendPayChan = SenderPaymentChannel
-- |Short-hand
type RecvPayChan = ReceiverPaymentChannel
-- |Short-hand
type RecvPayChanX = ReceiverPaymentChannelX
-- |Short-hand
class PaymentChannel a => PayChan a

