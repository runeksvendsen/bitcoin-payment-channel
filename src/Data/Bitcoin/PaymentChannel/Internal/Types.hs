{-# LANGUAGE DeriveGeneric, DataKinds #-}

module Data.Bitcoin.PaymentChannel.Internal.Types
(
    module Data.Bitcoin.PaymentChannel.Internal.Types
  , module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount
  , module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime
  , module Network.Haskoin.Transaction
  , module Network.Haskoin.Crypto
  , module Network.Haskoin.Script
)

where

import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime

import           Network.Haskoin.Transaction
import           Network.Haskoin.Crypto
import           Network.Haskoin.Script
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import           Data.Typeable
import           Data.Word
import qualified Data.Tagged as Tag


defaultConfig = Config defaultDustLimit defaultSettlementPeriod

defaultDustLimit = 700 :: BitcoinAmount
defaultSettlementPeriod = 10 :: Hour

newtype UnsignedPaymentTx = CUnsignedPaymentTx { unsignedTx :: HT.Tx } deriving Show
type FinalTx = HT.Tx

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
} deriving (Eq, Show, Typeable)

-- |Defines channel: sender, receiver, and expiration date
data ChannelParameters = CChannelParameters {
    cpSenderPubKey      ::  SendPubKey,
    cpReceiverPubKey    ::  RecvPubKey,
    -- |Channel expiration date/time
    cpLockTime          ::  BitcoinLockTime
} deriving (Eq, Show, Typeable)

-- |Holds information about the Bitcoin transaction used to fund
-- the channel
data FundingTxInfo = CFundingTxInfo {
    ftiHash         ::  HT.TxHash,      -- ^ Hash of funding transaction.
    ftiOutIndex     ::  Word32,         -- ^ Index/"vout" of funding output
    ftiOutValue     ::  BitcoinAmount   -- ^ Value of funding output (channel max value)
} deriving (Eq, Show, Typeable)

-- |Holds information about how to construct the payment transaction
data PaymentTxConfig = CPaymentTxConfig {
    -- |Value sender change address
    ptcSenderChangeAddress  ::  HC.Address
} deriving (Eq, Show, Typeable)

-- |Miscellaneous configuration options
data Config = Config {
    -- | Refuse to accept/produce payments with a client change value less than this amount.
    cDustLimit          :: BitcoinAmount
    -- | This many hours before the channel expiration date, consider the channel closed.
    --  This gives the receiver time to publish the settlement transaction, before the
    --  refund transaction becomes valid.
  , cSettlementPeriod   :: Hour
} deriving (Eq, Show, Typeable)

-- |Contains the bare minimum of information to transfer value from sender to receiver.
data Payment = CPayment {
    -- |Channel value remaining ('pcsValueLeft' of the state from which this payment was made)
    cpClientChange :: BitcoinAmount
    -- |Payment signature
  , cpSignature    :: PaymentSignature
} deriving (Eq, Typeable)

-- |Contains all information required to construct the payment transaction
data FullPayment = CFullPayment {
    fpPayment      :: Payment
    -- |The payment transaction redeems this outpoint
  , fpOutPoint     :: HT.OutPoint
  , fpRedeemScript :: HS.Script
    -- |Client change output address in the payment tx
  , fpChangeAddr   :: HC.Address
} deriving (Eq, Typeable)

-- |Contains payment signature plus sig hash flag byte
data PaymentSignature = CPaymentSignature {
    psSig       ::  HC.Signature
    -- |SigHash flag. Always "SigSingle True".
    ,psSigHash  ::  HS.SigHash
} deriving (Eq, Show, Typeable)

-- |Wraps a Network.Haskoin.Script.Script
newtype ChanScript = ChanScript { getScript :: HS.Script } deriving (Eq, Show)

-- Never confuse sender/receiver pubkey
newtype SendPubKey = MkSendPubKey {
    getSenderPK        ::  HC.PubKey
} deriving (Eq, Show)

newtype RecvPubKey = MkRecvPubKey {
    getReceiverPK        ::  HC.PubKey
} deriving (Eq, Show)

class IsPubKey a where
    getPubKey :: a -> HC.PubKey

instance IsPubKey SendPubKey where
    getPubKey = getSenderPK
instance IsPubKey RecvPubKey where
    getPubKey = getReceiverPK

type Hour = Tag.Tagged "Hour" Word32
toSeconds :: Hour -> Integer
toSeconds = fromIntegral . (* 3600) . Tag.unTagged
