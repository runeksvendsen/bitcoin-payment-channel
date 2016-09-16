module Data.Bitcoin.PaymentChannel.Internal.Error where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Util

data PayChanError =
    SigVerifyFailed |
    BadPaymentValue BitcoinAmount   |
    OutPointMismatch OutPoint |
    ChangeAddrMismatch Address |
    RedeemScriptMismatch Script |
    DustOutput BitcoinAmount |
    InternalError String

instance Show PayChanError where
    show SigVerifyFailed = "signature verification failed"
    show (BadPaymentValue valDiff) =
        "out-of-order payment (assigns " ++ show valDiff ++ " less value to receiver)"
    show (OutPointMismatch op) = "unexpected outpoint. expected: " ++ show op
    show (ChangeAddrMismatch addr) = "unexpected change address. expected: " ++ show addr
    show (RedeemScriptMismatch scr) = "unexpected redeem script. expected: " ++
        cs (serHex scr)
    show (DustOutput limit) = "server dust limit of " ++ show limit ++
        " not respected by client change output"
    show (InternalError e) = "Internal error: " ++ e

