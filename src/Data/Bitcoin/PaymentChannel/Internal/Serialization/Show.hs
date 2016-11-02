module Data.Bitcoin.PaymentChannel.Internal.Serialization.Show where

import           Data.Bitcoin.PaymentChannel.Internal.Serialization.Binary ()
import           Data.Bitcoin.PaymentChannel.Internal.Types
import           Data.Bitcoin.PaymentChannel.Internal.Util

import qualified Data.Serialize     as Bin

--- Misc.
instance Show Payment where
    show (CPayment val sig) =
        "<Payment: valLeft=" ++ show val ++
        ", sig=" ++ toHexString (Bin.encode sig) ++ ">"

instance Show FullPayment where
    show (CFullPayment p op script addr) =
        "<FullPayment: payment = " ++ show p ++ " " ++
        show (op, script, addr) ++ ">"