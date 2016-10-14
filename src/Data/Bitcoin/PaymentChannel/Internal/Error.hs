{-# LANGUAGE DeriveGeneric #-}

module Data.Bitcoin.PaymentChannel.Internal.Error where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Util
import           GHC.Generics

data PayChanError =
    SigVerifyFailed |
    BadPaymentValue BitcoinAmount   |
    OutPointMismatch OutPoint |
    ChangeAddrMismatch Address |
    RedeemScriptMismatch Script |
    DustOutput BitcoinAmount |
    PartialPaymentBadValue BitcoinAmount |
    ClosingPaymentBadValue |
    ChannelExpired
        deriving Generic

instance Show PayChanError where
    show SigVerifyFailed = "signature verification failed"
    show (BadPaymentValue valDiff) =
        "out-of-order payment (assigns " ++ show valDiff ++ " less value to receiver)"
    show (OutPointMismatch op) = "unexpected outpoint. expected: " ++ show op
    show (ChangeAddrMismatch addr) = "unexpected change address. expected: " ++ show addr
    show (RedeemScriptMismatch scr) = "unexpected redeem script. expected: " ++
        cs (serHex scr)
    show (PartialPaymentBadValue expVal) = "partial payment change value mismatch." ++
        " payment 1/2 with change value " ++ show expVal ++
        " was accepted, expecting second payment change value to match that."
    show (DustOutput limit) = "server dust limit of " ++ show limit ++
        " not respected by client change output"
    show ClosingPaymentBadValue = "payment not of zero value." ++
        " cannot receive value in closing payment."
    show ChannelExpired = "channel too close to expiration date"
