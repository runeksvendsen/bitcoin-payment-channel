{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.Bitcoin.PaymentChannel.Internal.Serialization.Orphans where

import           Data.Bitcoin.PaymentChannel.Internal.Util
import           Data.Bitcoin.PaymentChannel.Internal.Error

import qualified Data.Time.Clock    as Clock
import qualified Data.Time.Clock.POSIX as Clock


instance Serialize Clock.UTCTime where
    put t = putWord64be (round $ Clock.utcTimeToPOSIXSeconds t)
    get = Clock.posixSecondsToUTCTime . fromIntegral <$> getWord64be
