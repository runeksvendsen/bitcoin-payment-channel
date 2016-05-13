module Data.Bitcoin.PaymentChannel.Internal.Version where

import Data.Int

data Version = CVersion {
    versionMajor    ::  Int32,
    versionMinor    ::  Int32
}  deriving Show