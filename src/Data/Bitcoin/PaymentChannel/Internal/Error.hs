module Data.Bitcoin.PaymentChannel.Internal.Error where

data PayChanError =
    BadSignature |
    BadPayment   |
    NoValueTransferred

instance Show PayChanError where
    show BadSignature = "signature failed verification"
    show BadPayment = "payment amount to receiver less than previous payment"
    show NoValueTransferred = "cannot create payment Bitcoin transaction: no\
    \ value has been transferred yet"

