module PaymentChannel.Internal.Error.Internal where

import           Bitcoin.Types
import           Control.Exception
import           PaymentChannel.Internal.Error.User

data ReceiverError =
    BadSignatureInState
  | SettleSigningError BtcError
  | BadClosedServerChan PayChanError

instance Exception ReceiverError

instance Show ReceiverError where
    show BadSignatureInState =
        "Signature verification failed for in-state (alread-verified) payment. Data corruption?"
    show (BadClosedServerChan e) =
        "Bad payment in ClosedServerChan produced by 'acceptClosingPayment': " ++ show e
    show (SettleSigningError e) =
        "Settlement signing error: " ++ show e

