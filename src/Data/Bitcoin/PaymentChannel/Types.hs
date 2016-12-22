{-|
Module      : Data.Bitcoin.PaymentChannel.Types
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com

Types used with the interface provided by "Data.Bitcoin.PaymentChannel".

-}

{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

module Data.Bitcoin.PaymentChannel.Types
(
    -- *Sender/receiver channel info
    PaymentChannel(..), PayChan
  , PaymentChannelRecv(..)
  , getChannelFunding
  , getExpirationDate
  , getSenderPubKey
  , getReceiverPubKey
  , getFundingAmount
  , getPaymentCount
  , fundingAddress
  , getNewestPayment
  , getNewestSig
  , senderChangeValue
  , channelValueLeft
  , chanIsExhausted
  , expiresBefore
  , getChannelState

    -- *Sender state
  , SenderPaymentChannel(..), SendPayChan

    -- *Receiver state (simple)
  , ReceiverPaymentChannel, ReceiverPaymentChannelI(rpcMetadata), RecvPayChanI

    -- *Receiver state (with key metadata)
  , ReceiverPaymentChannelX, PayChanStatus(..)
  , S.mkExtendedKeyRPC, S.metaKeyIndex
  , S.getChannelStatus, S.setChannelStatus
  , S.markAsBusy, S.isReadyForPayment

    -- *Config
  , Config(..),defaultConfig
  , FundingTxInfo(..)
  , ChannelParameters(..), ChanParams

    -- *Payment
  , Payment,paySignature,payClientChange
  , FullPayment(..)
    -- **Error
  , PayChanError(..)

    -- *Bitcoin
  , BitcoinAmount
  , BitcoinLockTime(..)
  , module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Fee

    -- *Crypto
  , SendPubKey(..),RecvPubKey(..),IsPubKey(..),HasSendPubKey(..),HasRecvPubKey(..)

    -- *Util
  , fromDate, usesBlockHeight, dummyPayment

)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
import Data.Bitcoin.PaymentChannel.Internal.Metadata.Util
import Data.Bitcoin.PaymentChannel.Internal.Serialization ()

import qualified Data.Bitcoin.PaymentChannel.Internal.Receiver.Util as S
import qualified Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script as Script
import Data.Bitcoin.PaymentChannel.Internal.Error (PayChanError(..))
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Fee

import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Transaction as HT
import            Data.Word (Word64)


-- |Both sender and receiver state objects have a 'PaymentChannelState'
class HasPayChanState a where
    getPayChanState :: a -> PaymentChannelState

getChannelFunding :: HasPayChanState a => a -> HT.OutPoint
getChannelFunding = S.pcsFundingSource . getPayChanState

getExpirationDate :: HasPayChanState a => a -> BitcoinLockTime
getExpirationDate = S.pcsExpirationDate . getPayChanState

getSenderPubKey :: HasPayChanState a => a -> SendPubKey
getSenderPubKey = S.pcsClientPubKey . getPayChanState

getReceiverPubKey :: HasPayChanState a => a -> RecvPubKey
getReceiverPubKey = S.pcsServerPubKey . getPayChanState

getFundingAmount  :: HasPayChanState a => a -> BitcoinAmount
getFundingAmount = S.pcsFundingValue . getPayChanState

getPaymentCount :: HasPayChanState a => a -> Word64
getPaymentCount = pcsPaymentCount . getPayChanState

fundingAddress :: HasPayChanState a => a -> HC.Address
fundingAddress = Script.getP2SHFundingAddress . pcsParameters . getPayChanState

getNewestPayment :: HasPayChanState a => a -> Payment
getNewestPayment pcs = S.pcsGetPayment (getPayChanState pcs)

getNewestSig  :: HasPayChanState a => a -> HC.Signature
getNewestSig = psSig . paySignature . getNewestPayment

senderChangeValue :: HasPayChanState a => a -> BitcoinAmount
senderChangeValue = pcsClientChangeVal . getPayChanState

-- | Channel value left to send (subtracts dust limit from total available amount)
availableChannelVal :: HasPayChanState a => a -> BitcoinAmount
availableChannelVal = S.channelValueLeft . getPayChanState

-- |Returns 'True' if all available channel value has been transferred, 'False' otherwise
chanIsExhausted  :: HasPayChanState a => a -> Bool
chanIsExhausted = S.channelIsExhausted . getPayChanState

-- |Return True if channel expires earlier than given expiration date
expiresBefore :: HasPayChanState a => BitcoinLockTime -> a -> Bool
expiresBefore expDate chan = getExpirationDate chan < expDate

-- | Legay
getChannelState :: HasPayChanState a => a -> PaymentChannelState
getChannelState = getPayChanState

instance HasPayChanState SenderPaymentChannel where
    getPayChanState = spcState

instance HasPayChanState (ReceiverPaymentChannelI s) where
    getPayChanState = rpcState


-- |Get various information about an open payment channel.
class HasPayChanState a => PaymentChannel a where
    -- |Get amount received by receiver/left for sender
    valueToMe :: a -> BitcoinAmount
    -- |For internal use
    _setChannelState :: a -> PaymentChannelState -> a


-- |State object for the value sender
data SenderPaymentChannel = CSenderPaymentChannel {
    -- |Internal state object
    spcState     ::  PaymentChannelState,
    -- |Payment-signing function
    spcSignFunc  ::  HC.Hash256 -> HC.Signature
}

instance PaymentChannel SenderPaymentChannel where
    valueToMe = S.pcsClientChangeVal . spcState
    _setChannelState spc s = spc { spcState = s }

instance PaymentChannel (ReceiverPaymentChannelI s) where
    valueToMe (CReceiverPaymentChannel s _) =
        S.pcsFundingValue s - S.pcsClientChangeVal s
    _setChannelState rpc s = rpc { rpcState = s }

instance Show SenderPaymentChannel where
    show (CSenderPaymentChannel s _) =
        "<SenderPaymentChannel:\n\t" ++ show s ++ ">"


-- |Payment channel state objects with metadata information
class PaymentChannel a => PaymentChannelRecv a where
    -- |Get total amount (both settled and unsettled) sent by client
    clientTotalValueSent :: a -> BitcoinAmount

instance PaymentChannelRecv (ReceiverPaymentChannelI (MetadataI a)) where
    clientTotalValueSent = metaTotalValXfer . rpcMetadata




-- |Short-hand
type SendPayChan = SenderPaymentChannel
-- |Short-hand
class PaymentChannel a => PayChan a

