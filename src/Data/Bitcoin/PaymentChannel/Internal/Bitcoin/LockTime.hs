module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime where

import           Data.Bitcoin.PaymentChannel.Internal.Util
import           Data.Word (Word32)
-- import qualified Data.Serialize as Bin
-- import           Data.Serialize.Put (putWord32le)
-- import           Data.Serialize.Get (getWord32le)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Data.Time.Format ()    -- instance Show UTCTime

-- |Data type representing a Bitcoin LockTime, which specifies a point in time.
--  Derive a 'BitcoinLockTime' from a 'Data.Time.Clock.UTCTime' using 'fromDate'.
data BitcoinLockTime =
    -- |A value of "n" represents the point in time at which Bitcoin block number "n" appears
    LockTimeBlockHeight Word32 |
    -- |Specifies a point in time using a timestamp with 1-second accuracy
    LockTimeDate UTCTime deriving (Eq, Ord, Typeable)

instance Show BitcoinLockTime where
    show (LockTimeBlockHeight blockNum) = "block number " ++ show blockNum
    show (LockTimeDate date) = show date

instance Serialize BitcoinLockTime where
    put = putWord32le . toWord32
    get = parseBitcoinLocktime <$> getWord32le

-- | Convert from Bitcoin format ('Word32')
parseBitcoinLocktime :: Word32 -> BitcoinLockTime
parseBitcoinLocktime i
    | i <   500000000 = LockTimeBlockHeight i
    | i >=  500000000 = LockTimeDate $ posixSecondsToUTCTime (fromIntegral i)

-- | Convert to Bitcoin format (uint32 UNIX timestamp)
toWord32 :: BitcoinLockTime -> Word32
toWord32 (LockTimeBlockHeight i) = i
toWord32 (LockTimeDate date) =
    fromIntegral . round . utcTimeToPOSIXSeconds $ date

-- | Convert a 'Data.Time.Clock.UTCTime' to a 'BitcoinLockTime'
fromDate :: UTCTime -> BitcoinLockTime
fromDate = LockTimeDate

usesBlockHeight :: BitcoinLockTime -> Bool
usesBlockHeight (LockTimeBlockHeight _) = True
usesBlockHeight _ = False


instance ToJSON BitcoinLockTime where
    toJSON blt = Number $ scientific
        (fromIntegral $ toWord32 blt) 0

instance FromJSON BitcoinLockTime where
    parseJSON = withScientific "BitcoinLockTime" $
        fmap (parseBitcoinLocktime . fromIntegral) . parseJSONWord
