{-# LANGUAGE DeriveGeneric #-}

module PaymentChannel.Internal.Error.User where

import PaymentChannel.Internal.Error.Status     (HTTPError)
import Bitcoin.Compare
import PaymentChannel.Internal.Types
import PaymentChannel.Internal.Util
import           GHC.Generics

data PayChanError =
     SigVerifyFailed                -- ^ Signature verification failed
  |  BadSigHashFlag                 -- ^ Unexpected 'SigHash' flag. Expected: SIGHASH_SINGLE|ANYONECANPAY.
  |  BadPaymentValue BtcAmount      -- ^ Payment assigns less value to server than previous payment. Client change value is greater by the specified 'BtcAmount'.
  |  PaymentError (TxMismatch ChanParams)
  |  ClosingPaymentBadValue         -- ^ The closing payment only changes the payment transaction change address. Sending value is not allowed.
  |  ChannelExpired                 -- ^ Channel has expired or is too close to expiration date
  |  StatusError HTTPError          -- ^ Channel not ready for payment. 409=try again; 400=don't do that; 410=channel gone.
        deriving (Eq, Generic)

instance Show PayChanError where
    show SigVerifyFailed = "Signature verification failed"
    show BadSigHashFlag  = "Unexpected SIGHASH flag. Expected: SIGHASH_SINGLE|ANYONECANPAY (0x83)"
    show (BadPaymentValue valDiff) =
        "out-of-order payment (assigns " ++ show valDiff ++ " less value to receiver)"
    show ClosingPaymentBadValue = "payment not of zero value." ++
        " cannot receive value in closing payment."
    show ChannelExpired =
        "channel too close to expiration date"
    show (StatusError (_,e)) =
        "Status error:" ++ e
    show (PaymentError (TxInMismatch (InPrevOutMismatch op _))) =
        "unexpected outpoint. expected: " ++ show op
    show (PaymentError (TxOutMisMatch (OutAddressMismatch addr _))) =
        "unexpected change address. expected: " ++ show addr
    show (PaymentError (TxInMismatch (InRdmScrMismatch scr _))) =
        "unexpected redeemScript. expected: " ++ cs (serHex scr)
    show (PaymentError e) = "unexpected error: " ++ show e


-- instance Serialize PayChanError -- Generic PayChanError instance
