module Bitcoin.LockTime.Util
(
  module Bitcoin.LockTime.Util
, module Bitcoin.LockTime.Types
, Hour
)
where

import Bitcoin.LockTime.Types
import PaymentChannel.Internal.Config
import PaymentChannel.Internal.Util

import Data.Word (Word32)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format ()    -- instance Show UTCTime
import Control.Monad.Time


class HasLockTimeDate a where
    getLockTimeDate :: a -> LockTimeDate

isLocked :: (MonadTime m, HasLockTimeDate a) => a -> m Bool
isLocked a = do
    now <- fromIntegral . round . utcTimeToPOSIXSeconds <$> currentTime
    return $ now + toSeconds configSettlePeriod < toWord32 (getLockTimeDate a)

