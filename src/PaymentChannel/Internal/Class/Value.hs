{-# LANGUAGE FlexibleInstances #-}
module PaymentChannel.Internal.Class.Value where

import PaymentChannel.Internal.Payment.Types
import PaymentChannel.Internal.Payment
import PaymentChannel.Internal.Receiver.Types
import Bitcoin.Types


class HasValue a where
    valueOf :: a -> BtcAmount

instance HasValue SignedPayment where
    valueOf SigSinglePair{..} =
        btcInValue singleInput - nonDusty (btcAmount singleOutput)

instance HasValue (PayChanState BtcSig) where
    valueOf = valueOf . pcsPayment

instance HasValue a => HasValue [a] where
    valueOf = sum . map valueOf


