{-# LANGUAGE DeriveGeneric #-}

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
import           GHC.Generics

defaultDustLimit = 700 :: BitcoinAmount
defaultMinChanSize = defaultDustLimit * 2

newtype UnsignedPaymentTx = CUnsignedPaymentTx { unsignedTx :: HT.Tx } deriving Show
type FinalTx = HT.Tx

-- |Shared state object used by both value sender and value receiver.
data PaymentChannelState = CPaymentChannelState {
    -- |Defined by the sender and receiver
    pcsParameters           ::  ChannelParameters,
    -- |Retrieved by looking at the in-blockchain funding transaction
    pcsFundingTxInfo        ::  FundingTxInfo,
    pcsPaymentConfig        ::  PaymentTxConfig,
    -- |Value left to send (starts at @`-` dustLimit . ftiOutValue . pcsFundingTxInfo@)
    pcsValueLeft            ::  BitcoinAmount,
    -- |Signature over payment transaction of value 'pcsValueLeft'
    pcsPaymentSignature     ::  PaymentSignature
} deriving (Eq, Show, Typeable)

-- |Defines channel: sender, receiver, and expiration date
data ChannelParameters = CChannelParameters {
    cpSenderPubKey      ::  SendPubKey,
    cpReceiverPubKey    ::  RecvPubKey,
    -- |Channel expiration date/time
    cpLockTime          ::  BitcoinLockTime,
    -- |Use a per-channel dust limit, such that when the remaining channel value
    --  hits this limit (rather than zero), we say the channel is exhausted.
    --  This avoids the relatively complex SigSingle/SigNone logic, and reduces
    --  a payment to just a ANYONECANPAY|SigSingle signature with a corresponding
    --  change output, which is set to the channel funding address.
    cpDustLimit         ::  BitcoinAmount
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

-- |Used to transfer value from sender to receiver.
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
    -- |SigHash flag. Denotes whether the sender still wants its change output
    --  included in the settling transaction. In case of (SigNone True), the
    --  value sender has given up the rest of the channel value, leaving
    --  everything to the receiver. This is necessary so that no settling
    --  transaction containing an output below the "dust limit" is produced.
    ,psSigHash  ::  HS.SigHash
} deriving (Eq, Show, Typeable)


-- Never confuse sender/receiver pubkey again: let compiler check
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
