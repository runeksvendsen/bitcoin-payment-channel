module PaymentChannel.Internal.Error.Status where

import PaymentChannel.Internal.Metadata.Types

class HasHttpError a where
    getHttpError :: a -> HTTPError

type HTTPError = (Int,String)

-- instance HasHttpError PayChanStatus where
--

checkReadyForPayment :: PayChanStatus -> Maybe HTTPError
checkReadyForPayment ReadyForPayment        = Nothing
checkReadyForPayment PaymentInProgress      = Just (400, "Channel busy (payment in progress)")
checkReadyForPayment SettlementInProgress   = Just (409, "Settlement in progress")
checkReadyForPayment ChanClosed             = Just (410, "Channel closed")