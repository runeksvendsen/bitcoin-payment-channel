{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DataKinds #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.Bitcoin.PaymentChannel.Internal.Types
(
    module Data.Bitcoin.PaymentChannel.Internal.Types
  , module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount
  , module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime
  , module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
  , module Data.Bitcoin.PaymentChannel.Internal.Crypto.PubKey
  , module Network.Haskoin.Transaction
  , module Crypto
  , module Network.Haskoin.Script
) where

import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
import Data.Bitcoin.PaymentChannel.Internal.Crypto.PubKey

import           Network.Haskoin.Transaction
import           Network.Haskoin.Crypto as Crypto
import           Network.Haskoin.Script
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
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


type Hour = Tag.Tagged "Hour" Word32
toSeconds :: Hour -> Integer
toSeconds = fromIntegral . (* 3600) . Tag.unTagged

defaultConfig = Config defaultDustLimit defaultSettlementPeriod
dummyPayment = CPayment 0 ( CPaymentSignature dummySig (HS.SigAll True) )
    where dummySig = fromMaybe (error "BUG: Invalid dummy sig data") (HC.decodeDerSig hex2BS)
          hex2BS = fst $ B16.decode "304402204202cdb61cb702aa62de312a8e5eada817d90c4e26c8b696780b14d1576f204f02203c134d0acb057d917508ca9baab241a4f66ebea32f7acceeaf621a334927e17701"

defaultDustLimit = 6000 :: BitcoinAmount
defaultSettlementPeriod = 10 :: Hour


-- Serialization

instance Serialize ChanScript where
    put (ChanScript s) =
        putWord16be scriptBSLen >>
        putByteString scriptBS
            where scriptBS    = encode s
                  scriptBSLen = fromIntegral $ B.length scriptBS
    get = either error ChanScript . decode <$>
            (getWord16be >>=
             getByteString . fromIntegral)

instance Serialize PaymentChannelState where
    put (CPaymentChannelState cfg par fti payConf payCount valLeft sig) =
        put cfg >> put par >> put fti >> put payConf >> put payCount >>
        put valLeft >> put sig
    get = CPaymentChannelState <$> get <*> get <*>
        get <*> get <*> get <*> get <*> get

instance Serialize FundingTxInfo where
    put (CFundingTxInfo h idx val) =
        put h >> putWord32be idx >> put val
    get = CFundingTxInfo <$> get <*> getWord32be <*> get

instance Serialize PaymentTxConfig where
    put (CPaymentTxConfig sendAddr) =
        put sendAddr
    get = CPaymentTxConfig <$> get

instance Serialize Config where
    put (Config dl sp) =
        put dl >> put (Tag.unTagged sp)
    get = Config <$> get <*> fmap Tag.Tagged get

instance Serialize Payment where
    put (CPayment val sig) =
        put val >> put sig
    get = CPayment <$> get <*> get

instance Serialize FullPayment where
    put (CFullPayment p op cp addr) =
        put p >> put op >> put cp >> put addr
    get = CFullPayment <$> get <*> get <*> get <*> get

instance Serialize PaymentSignature where
    put (CPaymentSignature sig sigHash) =
        put sig >> put sigHash
    get = CPaymentSignature <$> get <*> get

-- Generic
instance ToJSON PaymentChannelState
instance FromJSON PaymentChannelState
instance ToJSON PaymentTxConfig
instance FromJSON PaymentTxConfig
instance ToJSON FundingTxInfo
instance FromJSON FundingTxInfo
instance ToJSON Config
instance FromJSON Config



instance ToJSON Payment where
    toJSON = object . paymentJSONLst

instance FromJSON Payment where
    parseJSON = withObject "Payment" parseJSONPayment

instance ToJSON PaymentSignature where
    toJSON = object . paySigJSONLst

instance FromJSON PaymentSignature where
    parseJSON = withObject "PaymentSignature" $ \o ->
        CPaymentSignature <$>
            (o .: "signature_data" >>= withText "SigDataHex" deserHex) <*>
            (o .: "sighash_flag"   >>= withText "SigHashFlagHex" deserHex)


paySigJSONLst :: PaymentSignature -> [Pair]
paySigJSONLst  (CPaymentSignature sig flag) =
    [ "signature_data"  .= String (serHex sig)
    , "sighash_flag"  .= String (serHex flag)
    ]

paymentJSONLst :: Payment -> [Pair]
paymentJSONLst (CPayment changeVal paySig) =
    ("change_value"      .= changeVal) :
        paySigJSONLst paySig


parseJSONPayment :: Object -> Parser Payment
parseJSONPayment o = CPayment
       <$>      o .: "change_value"
       <*>     (CPaymentSignature <$>
                   (o .: "signature_data" >>= withText "SigDataHex" deserHex) <*>
                   (o .: "sighash_flag"   >>= withText "SigHashFlagHex" deserHex))

instance ToJSON FullPayment where
    toJSON CFullPayment{..}
        | HT.OutPoint txid vout <- fpOutPoint = object $
            paymentJSONLst fpPayment ++
            [   "funding_txid"     .= txid
            ,   "funding_vout"     .= vout
            ,   "redeem_script"    .= fpChanParams
            ,   "change_address"   .= fpChangeAddr
            ]

instance FromJSON FullPayment where
    parseJSON = withObject "FullPayment" parseFullPayment

parseFullPayment :: Object -> Parser FullPayment
parseFullPayment o = CFullPayment
    <$>     parseJSONPayment o
    <*>     (HT.OutPoint <$>
                 o .: "funding_txid" <*>
                 o .: "funding_vout")
    <*>     (o .: "redeem_script")
    <*>      o .: "change_address"


--- Misc.
instance Show Payment where
    show (CPayment val sig) =
        "<Payment: valLeft=" ++ show val ++
        ", sig=" ++ toHexString (encode sig) ++ ">"

instance Show FullPayment where
    show (CFullPayment p op cp addr) =
        "<FullPayment: payment = " ++ show p ++ " " ++
        show (op, cp, addr) ++ ">"

