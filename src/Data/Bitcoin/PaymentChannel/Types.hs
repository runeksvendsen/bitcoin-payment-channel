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
BitcoinLockTime, fromDate,

-- Constants
dUST_LIMIT,
mIN_CHANNEL_SIZE
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
    (PaymentChannelState(..), Payment(..)
    ,FundingTxInfo(..), ChannelParameters(..),
    dUST_LIMIT, mIN_CHANNEL_SIZE)
import Data.Bitcoin.PaymentChannel.Internal.Serialization
--     ()
import Data.Bitcoin.PaymentChannel.Internal.Util
    (BitcoinAmount(..), toWord64,
    BitcoinLockTime, fromDate)
import Data.Bitcoin.PaymentChannel.Internal.State (pcsChannelID, pcsChannelTotalValue,
                                                   setClientChangeAddress)
import Data.Bitcoin.PaymentChannel.Internal.Error (PayChanError(..))

import qualified  Data.Binary as Bin
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Transaction as HT

-- |Get various information about an open payment channel.
class PaymentChannel a where
    -- |Get value sent to receiver/left for sender
    valueToMe           :: a -> BitcoinAmount
    channelValueLeft    :: a -> BitcoinAmount
    -- |Retrieve internal state object
    getChannelState     :: a -> PaymentChannelState
    -- |Get channel ID
    getChannelID        :: a -> HT.TxHash
    -- |Returns 'True' if all available channel value has been transferred, 'False' otherwise
    channelIsExhausted  :: a -> Bool
    -- |For internal use
    _setChannelState    :: a -> PaymentChannelState -> a

    channelValueLeft       = pcsValueLeft . getChannelState
    getChannelID           = pcsChannelID . getChannelState
    channelIsExhausted pch = pcsValueLeft (getChannelState pch) == 0 --TODO: payment sigHash == SigSingle




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
    -- |Used to verify signatures from value sender.
--     rpcVerifyFunc   ::  HC.Hash256 -> HC.PubKey -> HC.Signature -> Bool
    -- |Function which produces a signature that verifies against 'cpReceiverPubKey'.
    --  Used to produce the Bitcoin transaction that closes the channel.
--     rpcSignFunc     ::  HC.Hash256 -> HC.Signature
} deriving (Eq, Bin.Binary)

instance PaymentChannel SenderPaymentChannel where
    valueToMe (CSenderPaymentChannel s _) =
        pcsValueLeft s
    getChannelState = spcState
    _setChannelState spc s = spc { spcState = s }

instance PaymentChannel ReceiverPaymentChannel where
    valueToMe (CReceiverPaymentChannel s) =
        pcsChannelTotalValue s - pcsValueLeft s
    getChannelState = rpcState
    _setChannelState rpc s = rpc { rpcState = s }

instance Show SenderPaymentChannel where
    show (CSenderPaymentChannel s _) =
        "<SenderPaymentChannel:\n\t" ++ show s ++ ">"

instance Show ReceiverPaymentChannel where
    show (CReceiverPaymentChannel s) =
        "<ReceiverPaymentChannel:\n\t" ++ show s ++ ">"
