{-# LANGUAGE FlexibleInstances #-}
module Data.Bitcoin.PaymentChannel.Internal.Class.Value where

import Data.Bitcoin.PaymentChannel.Internal.Payment.Types
import Data.Bitcoin.PaymentChannel.Internal.Payment.Payment
import Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
import qualified Network.Haskoin.Transaction as HT


class HasValue a where
    valueOf :: a -> BitcoinAmount

instance Show a => HasValue (ClientSignedPaymentI a) where
    valueOf csp = fundVal csp - clientChangeVal
        where fundVal = chanFundingValue . cspPayment
              clientChangeVal = maybe
                (0 :: BitcoinAmount)            -- No client change output
                (fromIntegral . HT.outValue)    -- Client change output of value x
                (snd $ toUnsignedInMaybeOut csp)


instance HasValue PaymentChannelState where
    valueOf = valueOf . cPaymentFromState

instance HasValue (ReceiverPaymentChannelI a) where
    valueOf = valueOf . rpcState

instance HasValue a => HasValue [a] where
    valueOf = sum . map valueOf


