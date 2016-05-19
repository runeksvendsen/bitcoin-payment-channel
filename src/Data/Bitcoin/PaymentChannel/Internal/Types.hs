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
    pcsParameters           ::  ChannelParameters,
    pcsFundingTxInfo        ::  FundingTxInfo,
    pcsPaymentConfig        ::  PaymentTxConfig,
    -- |Value left to send
    pcsValueLeft            ::  BitcoinAmount,
    -- |Signature over payment transaction of value 'pcsValueLeft'
    --  unless no payment has been made yet
    pcsPaymentSignature     ::  Maybe PaymentSignature
} deriving (Eq, Show)

-- |Defines sender, receiver, and expiration date of the channel
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
    -- |Value sender change scriptPubKey in Bitcoin payment transaction
    ptcSenderChangeScript   ::  B.ByteString,
    -- |Value receiver destination scriptPubKey in Bitcoin payment transaction
    ptcReceiverChangeScript ::  B.ByteString
} deriving (Eq, Show)

-- |Used to transfer value from sender to receiver.
data Payment = CPayment {
    -- |Channel value remaining ('pcsValueLeft' of the state from which this payment was made)
    cpChannelValueLeft ::  BitcoinAmount,
    -- |Payment signature
    cpSignature        ::  PaymentSignature
}

-- |Contains payment signature plus sig hash flag byte
data PaymentSignature = CPaymentSignature {
    psSig       ::  HC.Signature
    ,psSigHash  ::  HS.SigHash
} deriving (Eq, Show)



-- Universally unique payment.
-- Contains the necessary information to identify the channel
-- over which the payment was sent, the signature (including SigHash flag)
-- and amount (for verification), as well as the protocol version.
-- data ChannelPayment = CChannelPayment {
--     -- |Payment protocol version (currently 2.x)
--     cpVersion       ::  Version,
--     -- |Channel ID is the regular Bitcoin double-SHA256 hash of the funding transaction
--     cpChannelID     ::  HT.TxHash,
--     cpPayment       ::  Payment,
--     -- |Optional info field
--     cpInfo          ::  Maybe BS.ByteString
-- }

instance Show Payment where
    show (CPayment val sig) =
        "<Payment: valLeft=" ++ show val ++ ">"
