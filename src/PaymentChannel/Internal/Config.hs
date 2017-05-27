{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module PaymentChannel.Internal.Config

where

import Bitcoin.Types
import Data.Word
import PaymentChannel.Internal.Util
import qualified RBPCP.Types          as RBPCP



class Monad m => HasConfSettlePeriod m where
    -- | The server is allowed to close the payment channel
    --    this many hours before the (in-blockchain) defined expiration date
    confSettlePeriod :: m Hour

-- | Various server-defined settings
data ServerSettings = ServerSettings
    { serverConfDustLimit     :: BtcAmount
    , serverConfSettlePeriod  :: Hour
    } deriving (Eq, Show, Typeable, Generic, Serialize, ToJSON, FromJSON, NFData)

fromFundingInfo :: RBPCP.FundingInfo -> ServerSettings
fromFundingInfo RBPCP.FundingInfo{..} =
    ServerSettings
        (fromIntegral fundingInfoDustLimit)
        (MkHour $ fromIntegral fundingInfoSettlement_period_hours)

newtype Hour = MkHour { numHours :: Word32 }
    deriving (Eq, Show, Typeable, Generic, Serialize, ToJSON, FromJSON, NFData)

toSeconds :: Num a => Hour -> a
toSeconds (MkHour i) = fromIntegral $ i * 3600


