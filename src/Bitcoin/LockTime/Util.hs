module Bitcoin.LockTime.Util
( module Bitcoin.LockTime.Util
, module Bitcoin.LockTime.Types
)
where

import Bitcoin.LockTime.Types
import Data.Time.Clock.POSIX
import Data.Time.Format ()    -- instance Show UTCTime
import Control.Monad.Time


class HasLockTimeDate a where
    getLockTimeDate :: a -> LockTimeDate

instance HasLockTimeDate LockTimeDate where
    getLockTimeDate = id

type Seconds = Word

isLocked :: (MonadTime m, HasLockTimeDate a)
    => Seconds    -- ^ No longer locked this number of seconds *before* actual expiration time.
                  --   Gives this many seconds to publish the settlement transaction before
                  --     actual lockTime expires.
    -> a
    -> m Bool
isLocked settlePeriodSeconds a = do
    now :: Integer <- round . utcTimeToPOSIXSeconds <$> currentTime
    return $ now + fromIntegral settlePeriodSeconds < fromIntegral (toWord32 $ getLockTimeDate a)
