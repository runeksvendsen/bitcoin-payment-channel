{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module PaymentChannel.Internal.Config
( module PaymentChannel.Internal.Config
, module PaymentChannel.Internal.Types.Hour
)
where

import Bitcoin.Types
import PaymentChannel.Internal.Util
import PaymentChannel.Internal.Types.Hour
import qualified RBPCP.Types          as RBPCP



class Monad m => HasConfSettlePeriod m where
    -- | The server is allowed to close the payment channel
    --    this many hours before the (in-blockchain) defined expiration date
    confSettlePeriod :: m Hour

-- | Various server-defined settings
data ServerSettings = ServerSettings
    { serverConfDustLimit     :: BtcAmount
      -- ^ The server will not accept a payment that leaves the client with less than this amount in change (unless it's exactly zero)
    , serverConfSettlePeriod  :: Hour
      -- ^ The channel may be closed this number of hours *before* the in-blockchain expiration date
    , serverConfMinDuration   :: Hour
      -- ^ Minimum duration of the payment channel
    , serverConfOpenPrice     :: BtcAmount
    -- ^ Value of the initial payment needed to open the payment channel
    } deriving (Eq, Show, Typeable, Generic, Serialize, ToJSON, FromJSON, NFData)

fromFundingInfo :: RBPCP.FundingInfo -> ServerSettings
fromFundingInfo RBPCP.FundingInfo{..} =
    ServerSettings
        (fromIntegral fundingInfoDustLimit)
        (MkHour $ fromIntegral fundingInfoSettlementPeriodHours)
        (MkHour $ fromIntegral fundingInfoMinDurationHours)
        (fromIntegral fundingInfoOpenPrice)
