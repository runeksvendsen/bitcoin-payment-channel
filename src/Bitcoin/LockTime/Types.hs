{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Bitcoin.LockTime.Types
(
  BtcLockTime(..)
, LockTimeDate(..)
, LockTimeBlockHeight(..)
, fromDate
)
where

import PaymentChannel.Internal.Util
import Data.Word (Word32)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format ()    -- instance Show UTCTime


-- |Data type representing a Bitcoin LockTime, which specifies a point in time.
class BtcLockTime a where
    parseLockTime :: Word32 -> Maybe a
    toWord32      :: a -> Word32

-- |Specifies a point in time using a timestamp with 1-second accuracy (till 2106-02-07)
data LockTimeDate = LockTimeDate Word32
    deriving (Eq, Ord, Typeable, Generic, NFData)

-- | A value of "n" represents the point in time at which Bitcoin block number "n" appears
data LockTimeBlockHeight = LockTimeBlockHeight Word32
    deriving (Eq, Ord, Typeable, Generic, NFData)

instance BtcLockTime LockTimeBlockHeight where
    toWord32 (LockTimeBlockHeight w) = w
    parseLockTime tstamp
            | tstamp > 0 && tstamp < 500000000 = Just $ LockTimeBlockHeight tstamp
            | otherwise = Nothing

instance BtcLockTime LockTimeDate where
    toWord32 (LockTimeDate w) = w
    parseLockTime = fromDate . posixSecondsToUTCTime . fromIntegral


-- | Convert a 'Data.Time.Clock.UTCTime' to a 'LockTimeDate'.
--   Returns 'Nothing' if the date is later than 2106-02-07
--    (or earlier than 1985-11-05).
fromDate :: UTCTime -> Maybe LockTimeDate
fromDate date
    | tstamp <- utcTimeToPOSIXSeconds date
    , tstamp >= 500000000 && round tstamp <= fromIntegral (maxBound :: Word32) =
        Just $ LockTimeDate $ fromIntegral (round tstamp)
    | otherwise = Nothing


instance ToJSON LockTimeBlockHeight where
    toJSON = encodeJSONLockTime
instance FromJSON LockTimeBlockHeight where
    parseJSON = withScientific "LockTimeBlockHeight" parseJSONLockTime

instance ToJSON LockTimeDate where
    toJSON = encodeJSONLockTime
instance FromJSON LockTimeDate where
    parseJSON = withScientific "LockTimeDate" parseJSONLockTime

parseJSONLockTime :: forall a. BtcLockTime a => Scientific -> Parser a
parseJSONLockTime sci =
    (parseLockTime <$> parseJSONWord sci) >>= maybe (fail "invalid locktime") return

encodeJSONLockTime :: BtcLockTime a => a -> Value
encodeJSONLockTime blt = Number $ scientific (fromIntegral $ toWord32 blt) 0

instance Show LockTimeBlockHeight where
    show (LockTimeBlockHeight blockNum) = "block number " ++ show blockNum

instance Show LockTimeDate where
    show (LockTimeDate i) = show $ posixSecondsToUTCTime (fromIntegral i)

instance Serialize LockTimeBlockHeight where
    put = putWord32le . toWord32
    get = parseBinLockTime

instance Serialize LockTimeDate where
    put = putWord32le . toWord32
    get = parseBinLockTime

parseBinLockTime :: forall a. BtcLockTime a => Get a
parseBinLockTime = getWord32le >>=
    maybe (fail "invalid locktime") return . parseLockTime
