{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PaymentChannel.Internal.Types.Hour where

import PaymentChannel.Internal.Util
import Data.Word

newtype Hour = MkHour { numHours :: Word32 }
    deriving
        ( Eq, Ord, Num, Enum, Real, Integral, Typeable
        , Generic, Serialize, ToJSON, FromJSON, NFData
        )

instance Show Hour where
    show (MkHour h)
      | h == 1 = "1 hour"
      | otherwise = show h ++ " hours"

toSeconds :: Num a => Hour -> a
toSeconds (MkHour i) = fromIntegral $ i * 3600


