{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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
    show (CFullPayment p op cp addr) =
        "<FullPayment: payment = " ++ show p ++ " " ++
        show (op, cp, addr) ++ ">"


instance Show ReceiverPaymentChannel where
    show (CReceiverPaymentChannel s _) =
        "<ReceiverPaymentChannel:\n\t" ++ show s ++ ">"

instance Show ReceiverPaymentChannelX where
    show (CReceiverPaymentChannel s m) =
        "<ReceiverPaymentChannelX:\n\t" ++ show s ++
        show m ++ ">"
