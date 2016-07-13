module Data.Bitcoin.PaymentChannel.Internal.Types where

import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Version

import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Script as HS
import qualified Data.ByteString as B
import Data.Word (Word32, Word64, Word8)
import qualified  Data.Binary as Bin
import qualified  Data.Binary.Put as BinPut
import qualified  Data.Binary.Get as BinGet

dUST_LIMIT = 700 :: BitcoinAmount
mIN_CHANNEL_SIZE = dUST_LIMIT * 2
pROTOCOL_VERSION = CVersion 2 0

newtype UnsignedPaymentTx = CUnsignedPaymentTx { unsignedTx :: HT.Tx } deriving Show
type FinalTx = HT.Tx

-- |Shared state object used by both value sender and value receiver.
data PaymentChannelState = CPaymentChannelState {
    -- |Defined by the sender and receiver
    pcsParameters           ::  ChannelParameters,
    -- |Retrieved by looking at the in-blockchain funding transaction
    pcsFundingTxInfo        ::  FundingTxInfo,

    pcsPaymentConfig        ::  PaymentTxConfig,
    -- |Value left to send (starts at @ftiOutValue . pcsFundingTxInfo@)
    pcsValueLeft            ::  BitcoinAmount,
    -- |Signature over payment transaction of value 'pcsValueLeft'
    --  unless no payment has been made yet
    pcsPaymentSignature     ::  Maybe PaymentSignature
} deriving (Eq, Show)

-- |Defines channel: sender, receiver, and expiration date
data ChannelParameters = CChannelParameters {
    cpSenderPubKey      ::  HC.PubKey,
    cpReceiverPubKey    ::  HC.PubKey,
    -- |Channel expiration date/time
    cpLockTime          ::  BitcoinLockTime
} deriving (Eq, Show)

-- |Holds information about the Bitcoin transaction used to fund
-- the channel
data FundingTxInfo = CFundingTxInfo {
    ftiHash         ::  HT.TxHash,      -- ^ Hash of funding transaction.
    ftiOutIndex     ::  Word32,         -- ^ Index/"vout" of funding output
    ftiOutValue     ::  BitcoinAmount   -- ^ Value of funding output (channel max value)
} deriving (Eq, Show)

-- |Holds information about how to construct the payment transaction
data PaymentTxConfig = CPaymentTxConfig {
    -- |Value sender change address
    ptcSenderChangeAddress  ::  HC.Address
} deriving (Eq, Show)

-- |Used to transfer value from sender to receiver.
data Payment = CPayment {
    -- |Channel value remaining ('pcsValueLeft' of the state from which this payment was made)
    cpChannelValueLeft ::  BitcoinAmount,
    -- |Payment signature
    cpSignature        ::  PaymentSignature
} deriving (Eq)

-- |Contains payment signature plus sig hash flag byte
data PaymentSignature = CPaymentSignature {
    psSig       ::  HC.Signature
    -- |SigHash flag. Denotes whether the sender still wants its change output
    --  included in the settling transaction. In case of (SigNone True), the
    --  value sender has given up the rest of the channel value, leaving
    --  everything to the receiver. This is necessary so that no settling
    --  transaction containing an output below the "dust limit" is produced.
    ,psSigHash  ::  HS.SigHash
} deriving (Eq, Show)


