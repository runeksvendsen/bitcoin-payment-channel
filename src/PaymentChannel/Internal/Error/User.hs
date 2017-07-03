{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PaymentChannel.Internal.Error.User where

import           Bitcoin.Compare
import           Bitcoin.LockTime.Types                (LockTimeParseError)
import           Control.Exception
import           GHC.Generics
import qualified Network.Haskoin.Script                as HS
import           PaymentChannel.Internal.Error.Status  (HTTPError)
import           PaymentChannel.Internal.Receiver.Open (OpenError)
import           PaymentChannel.Internal.Types
import           PaymentChannel.Internal.Util
import           PaymentChannel.RBPCP.Parse


data PayChanError =
     SigVerifyFailed                          -- ^ Signature verification failed
  |  LockTimeParseError LockTimeParseError
  |  BadSigHashFlag HS.SigHash HS.SigHash     -- ^ Unexpected 'SigHash' flag. Expected: SIGHASH_SINGLE|ANYONECANPAY.
  |  BadPaymentValue BtcAmount                -- ^ Payment assigns less value to server than previous payment. Client change value is greater by the specified 'BtcAmount'.
  |  PaymentError (TxMismatch ChanParams)
  |  ChannelExpired                           -- ^ Channel has expired or is too close to expiration date
  |  StatusError HTTPError                    -- ^ Channel not ready for payment. 409=try again; 410=channel gone.
  |  RBPCPError  ParseError                   -- ^ Failed to parse RBPCP payment
  |  OpenError   OpenError                    -- ^ Channel-open error
        deriving (Eq, Generic, NFData, ToJSON, FromJSON, Serialize)


class IsPayChanError e where
    mkChanErr :: e -> PayChanError

instance IsPayChanError LockTimeParseError where
    mkChanErr = LockTimeParseError

instance IsPayChanError HTTPError where
    mkChanErr = StatusError

instance IsPayChanError ParseError where
    mkChanErr = RBPCPError

instance IsPayChanError OpenError where
    mkChanErr = OpenError


instance Exception PayChanError


instance Show PayChanError where
    show SigVerifyFailed = "signature verification failed"
    show (LockTimeParseError e) = show e
    show (BadSigHashFlag sh1 sh2) =
        "unexpected SIGHASH flag: " ++ show sh1 ++
        ". expected: " ++ show sh2
    show (BadPaymentValue valDiff) =
        "out-of-order payment (assigns " ++ show valDiff ++ " less value to receiver)"
    show ChannelExpired =
        "channel too close to expiration date"
    show (StatusError he) = unwords
        ["status error:", show he]
    show (PaymentError (TxInMismatch (InPrevOutMismatch op _))) =
        "unexpected outpoint. expected: " ++ show op
    show (PaymentError (TxOutMisMatch (OutAddressMismatch addr _))) =
        "unexpected change address. expected: " ++ show addr
    show (PaymentError (TxInMismatch (InRdmScrMismatch scr _))) =
        "unexpected redeemScript. expected: " ++ cs (serHex scr)
    show (PaymentError e) = "unexpected error: " ++ show e
    show (RBPCPError pe) = "failed to parse payment: " ++ show pe
    show (OpenError oe) = "open error: " ++ show oe

-- instance Serialize PayChanError -- Generic PayChanError instance
