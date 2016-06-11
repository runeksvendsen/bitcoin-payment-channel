module Data.Bitcoin.PaymentChannel.Internal.Error where

data PayChanError =
    BadSignature |
    BadPaymentValue   |
    NoValueTransferred |
    DustOutput

instance Show PayChanError where
    show BadSignature = "signature failed verification"
    show BadPaymentValue = "payment amount to receiver less than previous payment"
    show DustOutput = "dust output in payment transaction"
    show NoValueTransferred = "cannot create payment Bitcoin transaction: no\
    \ value has been transferred yet"

