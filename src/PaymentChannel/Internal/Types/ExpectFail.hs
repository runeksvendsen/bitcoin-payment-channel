{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module PaymentChannel.Internal.Types.ExpectFail where

import PaymentChannel.Internal.Util

data ExpectFail a
  = a `FoundButExpected` a
      deriving (Eq, Generic, NFData, ToJSON, FromJSON, Serialize)

instance Show a => Show (ExpectFail a) where
    show (found `FoundButExpected` expec) = unwords
        ["found:", show found, "but expected:", show expec]
