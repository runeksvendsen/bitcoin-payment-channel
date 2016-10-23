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
    PaymentChannel(..), PayChan,
    SenderPaymentChannel(..), SendPayChan,
    ReceiverPaymentChannel, ReceiverPaymentChannelI, RecvPayChan,
    ReceiverPaymentChannelX, RecvPayChanX, S.mkExtendedKeyRPC, rpcGetXPub,
    Config(..),defaultConfig,
    Payment,cpSignature,
    FullPayment(..),
    FundingTxInfo(..),
    ChannelParameters, ChanParams,
    PayChanError(..),
    PaymentChannelState,
    SendPubKey(..),RecvPubKey(..),IsPubKey(..),
    BitcoinAmount,
    BitcoinLockTime(..), fromDate,
    usesBlockHeight
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Serialization ()

import qualified Data.Bitcoin.PaymentChannel.Internal.State as S
import qualified Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script as Script
import Data.Bitcoin.PaymentChannel.Internal.Error (PayChanError(..))

import qualified  Data.Serialize as Bin
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
    getFundingAmount   = S.pcsChannelTotalValue . getChannelState
    getPaymentCount    = pcsPaymentCount . getChannelState
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
    -- |Payment-signing function
    spcSignFunc  ::  HC.Hash256 -> HC.Signature
}

instance PaymentChannel SenderPaymentChannel where
    valueToMe = channelValueLeft
    getChannelState = spcState
    _setChannelState spc s = spc { spcState = s }

instance PaymentChannel ReceiverPaymentChannel where
    valueToMe rpc@(CReceiverPaymentChannel s _) =
        S.pcsChannelTotalValue s - channelValueLeft rpc
    getChannelState = rpcState
    _setChannelState rpc s = rpc { rpcState = s }

instance Bin.Serialize ReceiverPaymentChannel where
    put (CReceiverPaymentChannel rpc _ ) =
        Bin.put rpc >> Bin.putWord8 0x01
    get = CReceiverPaymentChannel <$> Bin.get <*> return ()

instance Bin.Serialize ReceiverPaymentChannelX where
    put (CReceiverPaymentChannel rpc pki ) =
        Bin.put rpc >> Bin.putWord8 0x02 >> Bin.put pki
    get = CReceiverPaymentChannel <$> Bin.get <*> Bin.get

instance Show SenderPaymentChannel where
    show (CSenderPaymentChannel s _) =
        "<SenderPaymentChannel:\n\t" ++ show s ++ ">"

instance Show ReceiverPaymentChannel where
    show (CReceiverPaymentChannel s _) =
        "<ReceiverPaymentChannel:\n\t" ++ show s ++ ">"

instance Show ReceiverPaymentChannelX where
    show (CReceiverPaymentChannel s _) =
        "<ReceiverPaymentChannelX:\n\t" ++ show s ++ ">"


-- Short-hands
type SendPayChan = SenderPaymentChannel
type RecvPayChan = ReceiverPaymentChannel
type RecvPayChanX = ReceiverPaymentChannelX
class PaymentChannel a => PayChan a

