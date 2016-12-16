{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
module Data.Bitcoin.PaymentChannel.Internal.Types
(
    module Data.Bitcoin.PaymentChannel.Internal.Types
  , module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount
  , module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime
  , module Data.Bitcoin.PaymentChannel.Internal.Crypto.PubKey
  , module Network.Haskoin.Transaction
  , module Crypto
  , module Network.Haskoin.Script
) where

import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime
import Data.Bitcoin.PaymentChannel.Internal.Crypto.PubKey

import           Network.Haskoin.Transaction
import           Network.Haskoin.Crypto as Crypto
import           Network.Haskoin.Script
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import qualified Data.ByteString.Base16 as B16
import           Data.Typeable
import           Data.Word
import qualified Data.Tagged as     Tag
import           GHC.Generics       (Generic)
import           Data.Maybe         (fromMaybe)


-- |Shared state object used by both value sender and value receiver.
data PaymentChannelState = CPaymentChannelState {
    -- |Holds various optional configuration options
    pcsConfig               ::  Config,
    -- |Defined by the sender and receiver
    pcsParameters           ::  ChannelParameters,
    -- |Retrieved by looking at the in-blockchain funding transaction
    pcsFundingTxInfo        ::  FundingTxInfo,
    pcsPaymentConfig        ::  PaymentTxConfig,
    pcsPaymentCount         ::  Word64,
    -- |Client change value
    pcsClientChangeVal      ::  BitcoinAmount,
    -- |Signature over payment transaction of value 'pcsValueLeft'
    pcsPaymentSignature     ::  PaymentSignature
} deriving (Eq, Show, Typeable, Generic)

instance HasSendPubKey PaymentChannelState where
    getSendPubKey = getSendPubKey . pcsParameters

instance HasRecvPubKey PaymentChannelState where
    getRecvPubKey = getRecvPubKey . pcsParameters


-- |Defines channel: sender, receiver, and expiration date
data ChannelParameters = CChannelParameters {
    cpSenderPubKey      ::  SendPubKey,
    cpReceiverPubKey    ::  RecvPubKey,
    -- |Channel expiration date/time
    cpLockTime          ::  BitcoinLockTime
} deriving (Eq, Show, Typeable, Generic)

instance HasSendPubKey ChannelParameters where
    getSendPubKey = cpSenderPubKey

instance HasRecvPubKey ChannelParameters where
    getRecvPubKey = cpReceiverPubKey

-- |Holds information about the Bitcoin transaction used to fund
-- the channel
data FundingTxInfo = CFundingTxInfo {
    ftiHash         ::  HT.TxHash,      -- ^ Hash of funding transaction.
    ftiOutIndex     ::  Word32,         -- ^ Index/"vout" of funding output (zero-based index of funding output within list of transaction outputs)
    ftiOutValue     ::  BitcoinAmount   -- ^ Value of funding output (channel max value).
} deriving (Eq, Show, Typeable, Generic)

-- |Holds information about how to construct the payment transaction
data PaymentTxConfig = CPaymentTxConfig {
    -- |Value sender change address
    ptcSenderChangeAddress  ::  HC.Address
} deriving (Eq, Show, Typeable, Generic)

-- |Miscellaneous configuration options
data Config = Config {
    -- | Refuse to accept/produce payments with a client change value less than this amount.
    cDustLimit          :: BitcoinAmount
    -- | This many hours before the channel expiration date, consider the channel closed.
    --  This gives the receiver time to publish the settlement transaction, before the
    --  refund transaction becomes valid.
  , cSettlementPeriod   :: Hour
} deriving (Eq, Show, Typeable, Generic)

-- |Contains the bare minimum of information to transfer value from sender to receiver.
data Payment = CPayment {
    -- |Channel value remaining ('pcsValueLeft' of the state from which this payment was made)
    payClientChange :: BitcoinAmount
    -- |Get payment signature from payment
  , paySignature    :: PaymentSignature
} deriving (Eq, Typeable)

-- |Contains all information required to construct the payment transaction
data FullPayment = CFullPayment {
    fpPayment       :: Payment
    -- |The payment transaction redeems this outpoint
  , fpOutPoint      :: HT.OutPoint
    -- |Using the redeemScript produced by getRedeemScript from these 'ChannelParameters'
  , fpChanParams    :: ChannelParameters
    -- |Paying this amount to the client/sender change output
  , fpChangeAddr    :: HC.Address
} deriving (Eq, Typeable)

instance HasSendPubKey FullPayment where
    getSendPubKey = getSendPubKey . fpChanParams

instance HasRecvPubKey FullPayment where
    getRecvPubKey = getRecvPubKey . fpChanParams


-- |Contains payment signature plus sig hash flag byte
data PaymentSignature = CPaymentSignature {
    psSig       ::  HC.Signature
    -- |SigHash flag. Always "SigSingle True".
    ,psSigHash  ::  HS.SigHash
} deriving (Eq, Show, Typeable)

-- |Wraps a Network.Haskoin.Script.Script
newtype ChanScript = ChanScript { getScript :: HS.Script } deriving (Eq, Show)

type PayChanState  = PaymentChannelState
type ChanParams = ChannelParameters

-- |ReceiverPaymentChannel without metadata
type ReceiverPaymentChannel = ReceiverPaymentChannelI ()
-- |ReceiverPaymentChannel with BIP32, "extended key" index as metadata
type ReceiverPaymentChannelX = ReceiverPaymentChannelI Metadata

-- |State object for the value receiver. "meta" is the metadata type,
--  which is '()' in the case of 'ReceiverPaymentChannel'
data ReceiverPaymentChannelI meta = CReceiverPaymentChannel {
    -- |Internal state object
    rpcState    :: PaymentChannelState
  , rpcMetadata :: meta
} deriving (Eq, Typeable)

instance HasSendPubKey (ReceiverPaymentChannelI a) where
    getSendPubKey = getSendPubKey . rpcState

instance HasRecvPubKey (ReceiverPaymentChannelI a) where
    getRecvPubKey = getRecvPubKey . rpcState


type Hour = Tag.Tagged "Hour" Word32
toSeconds :: Hour -> Integer
toSeconds = fromIntegral . (* 3600) . Tag.unTagged

-- |Key index for a BIP32 key
type KeyDeriveIndex = Word32

data Metadata = Metadata
    { mdKeyIndex        :: KeyDeriveIndex
    , mdValueReceived   :: BitcoinAmount
    , mdChannelStatus   :: PayChanStatus
    } deriving (Eq, Typeable, Show, Generic)

data PayChanStatus =
    ReadyForPayment
  | PaymentInProgress
  | SettlementInProgress
  | ChanClosed
    deriving (Eq, Typeable, Show, Read, Generic)

metaSetStatus :: PayChanStatus -> Metadata -> Metadata
metaSetStatus s md = md { mdChannelStatus = s }

metaGetStatus :: Metadata -> PayChanStatus
metaGetStatus Metadata{ mdChannelStatus = s } = s

defaultConfig = Config defaultDustLimit defaultSettlementPeriod
dummyPayment = CPayment 0 ( CPaymentSignature dummySig (HS.SigAll True) )
    where dummySig = fromMaybe (error "BUG: Invalid dummy sig data") (HC.decodeDerSig hex2BS)
          hex2BS = fst $ B16.decode "304402204202cdb61cb702aa62de312a8e5eada817d90c4e26c8b696780b14d1576f204f02203c134d0acb057d917508ca9baab241a4f66ebea32f7acceeaf621a334927e17701"

defaultDustLimit = 700 :: BitcoinAmount
defaultSettlementPeriod = 10 :: Hour

-- |Short-hand
type RecvPayChan = ReceiverPaymentChannel
-- |Short-hand
type RecvPayChanX = ReceiverPaymentChannelX

