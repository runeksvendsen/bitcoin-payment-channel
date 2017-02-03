{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Bitcoin.Error where

import Bitcoin.Amount
import Bitcoin.Util

data BtcError =
    InsufficientFunds   { eAmountMissing    :: BtcAmount }
  | DustOutput          { eDustLimit        :: BtcAmount }
        deriving (Eq, Show, Typeable, Generic, NFData, ToJSON, FromJSON, Serialize)
