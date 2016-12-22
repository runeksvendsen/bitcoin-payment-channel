{-# LANGUAGE DeriveAnyClass #-}
module Data.Bitcoin.PaymentChannel.Internal.Metadata.Types where

import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Settlement.Types
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount
import Data.Bitcoin.PaymentChannel.Internal.Crypto.PubKey

import Data.Time.Clock    (UTCTime)
import GHC.Generics       (Generic)
import Data.Typeable


data MetadataI kd = Metadata
    { mdKeyData         :: kd
    , mdSettledValue    :: [SettleInfo]
    , mdUnsettledValue  :: BitcoinAmount
    , mdChannelStatus   :: PayChanStatus
    } deriving (Eq, Typeable, Show, Generic)

type MetadataIdx = MetadataI KeyDeriveIndex

data PayChanStatus =
    ReadyForPayment
  | PaymentInProgress
  | SettlementInProgress
  | ChanClosed
    deriving (Eq, Typeable, Show, Read, Generic, Serialize)

instance Serialize a => Serialize (MetadataI a) where
    put (Metadata ki valS valU md) =
        put ki >> put valS >> put valU >> put md
    get = Metadata <$> get <*> get <*> get <*> get

-- instance  PayChanStatus -- Generic PayChanError instance

instance ToJSON a => ToJSON (MetadataI a)
instance FromJSON a => FromJSON (MetadataI a)



instance ToJSON PayChanStatus where
    toJSON = String . cs . show

instance FromJSON PayChanStatus where
    parseJSON = withText "PayChanStatus" (return . read . cs)

