{-|
Module      : Data.Bitcoin.PaymentChannel.Types
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com

Types used with the interface provided by "Data.Bitcoin.PaymentChannel".

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bitcoin.PaymentChannel.Types
(
PaymentChannel(..),
SenderPaymentChannel(..),
ReceiverPaymentChannel(..),
Payment,cpSignature,
FullPayment(..),
FundingTxInfo(..),
ChannelParameters(..),
PayChanError(..),
PaymentChannelState,
SendPubKey(..),RecvPubKey(..),IsPubKey(..),
BitcoinAmount,
BitcoinLockTime(..), fromDate,
usesBlockHeight,

-- Util
b64Encode,
-- Constants
defaultDustLimit,
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Serialization

import qualified Data.Bitcoin.PaymentChannel.Internal.State as S
import qualified Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script as Script
import Data.Bitcoin.PaymentChannel.Internal.Error (PayChanError(..))

import qualified  Data.Serialize as Bin
import            Data.Aeson as JSON -- (FromJSON, ToJSON)
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Transaction as HT
-- import qualified  Network.Haskoin.Transaction as HS


-- |Get various information about an open payment channel.
class PaymentChannel a where
    -- |Get value sent to receiver/left for sender
    valueToMe           :: a -> BitcoinAmount
    -- |Retrieve internal state object
    getChannelState     :: a -> PaymentChannelState
    getChannelFunding   :: a -> HT.OutPoint
    getExpirationDate   :: a -> BitcoinLockTime
    getSenderPubKey     :: a -> SendPubKey
    getFundingAmount    :: a -> BitcoinAmount
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
    getFundingAmount   = S.pcsChannelTotalValue . getChannelState
    fundingAddress  = Script.getP2SHFundingAddress . pcsParameters . getChannelState
    senderChangeValue  = pcsClientChangeVal . getChannelState
    channelValueLeft   = S.channelValueLeft . getChannelState
    channelIsExhausted = S.channelIsExhausted . getChannelState
    expiresBefore expDate chan = getExpirationDate chan < expDate
    getNewestPayment pcs = S.pcsGetPayment (getChannelState pcs)
    getNewestSig = psSig . cpSignature . getNewestPayment

-- |State object for the value sender
data SenderPaymentChannel = CSenderPaymentChannel {
    -- |Internal state object
    spcState     ::  PaymentChannelState,
    spcSignFunc  ::  HC.Hash256 -> HC.Signature
}

-- |State object for the value receiver
newtype ReceiverPaymentChannel = CReceiverPaymentChannel {
    -- |Internal state object
    rpcState        ::  PaymentChannelState
} deriving (Eq, Bin.Serialize, FromJSON, ToJSON)

instance PaymentChannel SenderPaymentChannel where
    valueToMe = channelValueLeft
    getChannelState = spcState
    _setChannelState spc s = spc { spcState = s }

instance PaymentChannel ReceiverPaymentChannel where
    valueToMe rpc@(CReceiverPaymentChannel s) =
        S.pcsChannelTotalValue s - channelValueLeft rpc
    getChannelState = rpcState
    _setChannelState rpc s = rpc { rpcState = s }

instance Show SenderPaymentChannel where
    show (CSenderPaymentChannel s _) =
        "<SenderPaymentChannel:\n\t" ++ show s ++ ">"

instance Show ReceiverPaymentChannel where
    show (CReceiverPaymentChannel s) =
        "<ReceiverPaymentChannel:\n\t" ++ show s ++ ">"
