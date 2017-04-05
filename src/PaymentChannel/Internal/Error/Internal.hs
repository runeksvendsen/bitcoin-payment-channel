module PaymentChannel.Internal.Error.Internal where

import Bitcoin.Types

data ReceiverError =
    SettleError BtcError


instance Show ReceiverError where
    show (SettleError e) =
        "Settlement error: " ++ show e



