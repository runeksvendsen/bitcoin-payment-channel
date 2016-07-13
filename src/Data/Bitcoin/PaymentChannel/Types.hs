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
Payment,
FundingTxInfo(..),
ChannelParameters(..),
PayChanError(..),
PaymentChannelState,

BitcoinAmount, toWord64,
BitcoinLockTime(..), fromDate,
usesBlockHeight,

-- Util
b64Encode,
-- Constants
dUST_LIMIT,
mIN_CHANNEL_SIZE
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
    (PaymentChannelState(..), Payment(..)
    ,FundingTxInfo(..), ChannelParameters(..),
    PaymentSignature(..),
    dUST_LIMIT, mIN_CHANNEL_SIZE)
import Data.Bitcoin.PaymentChannel.Internal.Serialization
--     ()
import Data.Bitcoin.PaymentChannel.Internal.Util
    (BitcoinAmount(..), toWord64,
    BitcoinLockTime(..), fromDate)
import qualified Data.Bitcoin.PaymentChannel.Internal.State as S (pcsChannelID, pcsChannelTotalValue,
                                                   setClientChangeAddress,
                                                   channelValueLeft, channelIsExhausted, pcsExpirationDate)
import Data.Bitcoin.PaymentChannel.Internal.Error (PayChanError(..))
-- import Data.Bitcoin.PaymentChannel.Internal.Types ()
import qualified  Data.Binary as Bin
import            Data.Aeson as JSON -- (FromJSON, ToJSON)
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Script as HS

-- |Get various information about an open payment channel.
class PaymentChannel a where
    -- |Get value sent to receiver/left for sender
    valueToMe           :: a -> BitcoinAmount
    -- |Retrieve internal state object
    getChannelState     :: a -> PaymentChannelState
    getChannelID        :: a -> HT.OutPoint
    getExpirationDate   :: a -> BitcoinLockTime
    -- |For internal use
    _setChannelState    :: a -> PaymentChannelState -> a

    channelValueLeft    :: a -> BitcoinAmount
    -- |Returns 'True' if all available channel value has been transferred, 'False' otherwise
    channelIsExhausted  :: a -> Bool

    getChannelID       = S.pcsChannelID . getChannelState
    getExpirationDate  = S.pcsExpirationDate . getChannelState

    channelValueLeft   = S.channelValueLeft . getChannelState
    channelIsExhausted = S.channelIsExhausted . getChannelState


-- |State object for the value sender
data SenderPaymentChannel = CSenderPaymentChannel {
    -- |Internal state object
    spcState     ::  PaymentChannelState,
    -- |Used to sign payments from sender.
    --  This function, when given a 'HC.Hash256', produces a signature that
    --  verifies against 'cpSenderPubKey'.
    spcSignFunc  ::  HC.Hash256 -> HC.Signature
}

-- |State object for the value receiver
newtype ReceiverPaymentChannel = CReceiverPaymentChannel {
    -- |Internal state object
    rpcState        ::  PaymentChannelState
} deriving (Eq, Bin.Binary, FromJSON, ToJSON)

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
