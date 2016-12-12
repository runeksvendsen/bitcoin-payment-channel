module Data.Bitcoin.PaymentChannel.Internal.Error.Status where

import Data.Bitcoin.PaymentChannel.Internal.Types

type HTTPError = (Int,String)

checkReadyForPayment :: PayChanStatus -> Maybe HTTPError
checkReadyForPayment ReadyForPayment        = Nothing
checkReadyForPayment PaymentInProgress      = Just (400, "Channel busy (payment in progress)")
checkReadyForPayment SettlementInProgress   = Just (409, "Settlement in progress")
checkReadyForPayment (ChannelClosed _)      = Just (410, "Channel closed")