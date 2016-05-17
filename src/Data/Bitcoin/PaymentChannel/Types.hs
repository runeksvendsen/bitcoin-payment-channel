{-|
Module      : Data.Bitcoin.PaymentChannel.Types
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com

Types used with the interface provided by "Data.Bitcoin.PaymentChannel".

-}

module Data.Bitcoin.PaymentChannel.Types
(
SenderPaymentChannel(..),
ReceiverPaymentChannel(..),
Payment,
FundingTxInfo(..),
ChannelParameters(..),
PayChanError,

BitcoinAmount, toWord64,
PaymentChannel(..),
BitcoinLockTime, fromDate
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
    (PaymentChannelState(..), Payment(..)
    ,FundingTxInfo(..), ChannelParameters(..))
import Data.Bitcoin.PaymentChannel.Internal.Serialization
--     ()
import Data.Bitcoin.PaymentChannel.Internal.Util
    (BitcoinAmount, toWord64,
    BitcoinLockTime, fromDate)
import Data.Bitcoin.PaymentChannel.Internal.State (pcsChannelID, pcsChannelTotalValue)
import Data.Bitcoin.PaymentChannel.Internal.Error (PayChanError(..))

import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Transaction as HT


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
data ReceiverPaymentChannel = CReceiverPaymentChannel {
    -- |Internal state object
    rpcState        ::  PaymentChannelState,
    -- |Used to verify signatures from value sender.
    rpcVerifyFunc   ::  HC.Hash256 -> HC.PubKey -> HC.Signature -> Bool,
    -- |Function which produces a signature that verifies against 'cpReceiverPubKey'.
    --  Used to produce the Bitcoin transaction that closes the channel.
    rpcSignFunc     ::  HC.Hash256 -> HC.Signature
}

-- |Get various information about an open payment channel.
class PaymentChannel a where
    -- |Get value sent to receiver/left for sender
    valueToMe           :: a -> BitcoinAmount
    -- |Retrieve internal channel state
    getChannelState     :: a -> PaymentChannelState
    -- |Get channel ID
    getChannelID        :: a -> HT.TxHash
    -- |Returns 'True' if all available channel value has been transferred, 'False' otherwise
    channelIsExhausted  :: a -> Bool

    getChannelID           = pcsChannelID . getChannelState
    channelIsExhausted pch = pcsValueLeft (getChannelState pch) == 0 --TODO: payment sigHash == SigSingle

instance PaymentChannel SenderPaymentChannel where
    valueToMe (CSenderPaymentChannel s _) =
        pcsValueLeft s
    getChannelState = spcState

instance PaymentChannel ReceiverPaymentChannel where
    valueToMe (CReceiverPaymentChannel s _ _) =
        pcsChannelTotalValue s - pcsValueLeft s
    getChannelState = rpcState


instance Show SenderPaymentChannel where
    show (CSenderPaymentChannel s _) =
        "<SenderPaymentChannel:\n\t" ++ show s ++ ">"

instance Show ReceiverPaymentChannel where
    show (CReceiverPaymentChannel s _ _) =
        "<ReceiverPaymentChannel:\n\t" ++ show s ++ ">"
