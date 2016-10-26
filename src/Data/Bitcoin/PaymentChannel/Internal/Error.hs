{-# LANGUAGE DeriveGeneric #-}

module Data.Bitcoin.PaymentChannel.Internal.Error where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Util
import           GHC.Generics

data PayChanError =
     SigVerifyFailed                -- ^Signature verification failed
  |  BadPaymentValue BitcoinAmount  -- ^Payment assigns less value to server than previous payment. Client change value is greater by the specified 'BitcoinAmount'.
  |  OutPointMismatch OutPoint      -- ^'Network.Haskoin.Transaction.OutPoint' in payment does not match the one in server's state
  |  ChangeAddrMismatch Address     -- ^Client change 'Network.Haskoin.Crypto.Address' in payment does not match the one in server's state
  |  RedeemScriptMismatch Script    -- ^redeemScript in payment does not match the one in server's state
  |  DustOutput BitcoinAmount       -- ^Client change value is less than dust limit (payment transaction would contain a dust output)
  |  ClosingPaymentBadValue         -- ^The closing payment only changes the payment transaction change address. Sending value is not allowed.
  |  ChannelExpired                 -- ^Channel has expired or is too close to expiration date
        deriving Generic

instance Show PayChanError where
    show SigVerifyFailed = "Signature verification failed"
    show (BadPaymentValue valDiff) =
        "out-of-order payment (assigns " ++ show valDiff ++ " less value to receiver)"
    show (OutPointMismatch op) = "unexpected outpoint. expected: " ++ show op
    show (ChangeAddrMismatch addr) = "unexpected change address. expected: " ++ show addr
    show (RedeemScriptMismatch scr) = "unexpected redeem script. expected: " ++
        cs (serHex scr)
    show (DustOutput limit) = "server dust limit of " ++ show limit ++
        " not respected by client change output"
    show ClosingPaymentBadValue = "payment not of zero value." ++
        " cannot receive value in closing payment."
    show ChannelExpired = "channel too close to expiration date"
