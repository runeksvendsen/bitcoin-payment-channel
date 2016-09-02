module Data.Bitcoin.PaymentChannel.Internal.Error where

import Data.Bitcoin.PaymentChannel.Internal.Types (BitcoinAmount)

data PayChanError =
    SigVerifyFailed |
    BadPaymentValue BitcoinAmount   |
    DustOutput |
    InternalError String

instance Show PayChanError where
    show SigVerifyFailed = "signature verification failed"
    show (BadPaymentValue valDiff) =
        "out-of-order payment (assigns " ++ show valDiff ++ " less value to receiver)"
    show DustOutput = "dust output in payment transaction"
    show (InternalError e) = "Internal error: " ++ e

