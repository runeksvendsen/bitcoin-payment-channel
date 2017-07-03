{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PaymentChannel.Internal.Serialization.Orphans where

import           PaymentChannel.Internal.Error
import           PaymentChannel.Internal.Util

import qualified Data.Time.Clock               as Clock
import qualified Data.Time.Clock.POSIX         as Clock


instance Serialize Clock.UTCTime where
    put t = putWord64be (round $ Clock.utcTimeToPOSIXSeconds t)
    get = Clock.posixSecondsToUTCTime . fromIntegral <$> getWord64be
