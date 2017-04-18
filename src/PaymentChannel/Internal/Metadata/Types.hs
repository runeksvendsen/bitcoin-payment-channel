{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module PaymentChannel.Internal.Metadata.Types where

import PaymentChannel.Internal.Util
import PaymentChannel.Internal.Settlement.Types
import Bitcoin.Amount
import PaymentChannel.Internal.Crypto.PubKey

import Data.Time.Clock    (UTCTime)
import GHC.Generics       (Generic)
import Data.Typeable


data MetadataI kd = Metadata
    { mdKeyData         :: kd
    , mdPayCount        :: Word64
    , mdSettledValue    :: [SettleInfo]
    , mdUnsettledValue  :: BtcAmount
    , mdChannelStatus   :: PayChanStatus
    } deriving (Eq, Typeable, Show, Generic, NFData)

initialMetadata :: MetadataI ()
initialMetadata = Metadata () 0 [] 0 ReadyForPayment

type MetadataIdx = MetadataI KeyDeriveIndex

data PayChanStatus =
    ReadyForPayment
  | PaymentInProgress
  | SettlementInProgress
  | ChannelClosed SignedPayment  -- ^ The closing channel payment
    deriving (Eq, Typeable, Show, Generic, ToJSON, FromJSON, Serialize, NFData)

instance Serialize a => Serialize (MetadataI a) where
    put Metadata{..} =
           put mdKeyData
        >> put mdPayCount
        >> put mdSettledValue
        >> put mdUnsettledValue
        >> put mdChannelStatus
    get = Metadata <$> get <*> get <*> get <*> get <*> get

-- Generic
instance ToJSON a   => ToJSON (MetadataI a)
instance FromJSON a => FromJSON (MetadataI a)


-- instance ToJSON PayChanStatus where
--     toJSON = String . cs . show

--instance FromJSON PayChanStatus where
--    parseJSON = withText "PayChanStatus" $ \txt -> -- (return . read . cs)
