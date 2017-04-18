{-# LANGUAGE DeriveAnyClass #-}
module PaymentChannel.Internal.Error.Status where

import PaymentChannel.Internal.Types
import PaymentChannel.Internal.Metadata.Types
import Control.Exception

class HasHttpError a where
    getHttpError :: a -> HTTPError

data HTTPError = HTTPError { heCode :: Int, heStatus :: String }
    deriving (Eq, Generic, NFData, ToJSON, FromJSON, Serialize)

instance Show HTTPError where
    show (HTTPError c e) = unwords
        ["<HTTPError", show c ++ ":", e ++ ">"]

instance Exception HTTPError

instance HasHttpError HTTPError where
    getHttpError = id

checkReadyForPayment :: PayChanStatus -> Maybe HTTPError
checkReadyForPayment ReadyForPayment        = Nothing
checkReadyForPayment PaymentInProgress      = Just $ HTTPError 400 "Channel busy (payment in progress)"
checkReadyForPayment SettlementInProgress   = Just $ HTTPError 409 "Settlement in progress"
checkReadyForPayment (ChannelClosed _)      = Just $ HTTPError 410 "Channel closed"
