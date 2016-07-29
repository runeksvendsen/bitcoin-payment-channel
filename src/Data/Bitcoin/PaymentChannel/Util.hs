{-|
Module      : Data.Bitcoin.PaymentChannel.Types
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com

Utility functions for "Data.Bitcoin.PaymentChannel".

-}

module Data.Bitcoin.PaymentChannel.Util
(
getFundingAddress,
setSenderChangeAddress,

serialize, serialize',
BitcoinLockTime, parseBitcoinLocktime, toWord32, fromDate, deserEither,
parseJSONInt,

unsafeUpdateRecvState
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
    (PaymentChannelState(..), PaymentTxConfig(..),
    Payment(..))
-- import Data.Bitcoin.PaymentChannel.Internal.State
--     (ptcSenderChangeAddress)
import Data.Bitcoin.PaymentChannel.Internal.Script
    (getP2SHFundingAddress)
import Data.Bitcoin.PaymentChannel.Internal.Util
    (BitcoinLockTime, parseBitcoinLocktime, toWord32, fromDate, deserEither,
     serialize, serialize')
import Data.Bitcoin.PaymentChannel.Internal.State
    (setClientChangeAddress)
import Data.Bitcoin.PaymentChannel.Internal.Serialization
    (parseJSONInt)

import Data.Bitcoin.PaymentChannel.Types
--     (ChannelParameters, FundingTxInfo)

import qualified Network.Haskoin.Crypto as HC

-- | Derive a Bitcoin address, for funding a payment channel, from
--  'ChannelParameters'.
--  The transaction which pays to this address is the channel funding transaction,
--  and information about this transaction is contained in
--  'FundingTxInfo'.
getFundingAddress = getP2SHFundingAddress

-- |Set new value sender change address
setSenderChangeAddress :: PaymentChannel a => a -> HC.Address -> a
setSenderChangeAddress pch addr =
    _setChannelState pch (setClientChangeAddress (getChannelState pch) addr)

-- |Update internal state without signature verification.
-- Useful for database-type services where a logic layer has already
--  verified the signature, and it just needs to be stored.
unsafeUpdateRecvState :: ReceiverPaymentChannel -> Payment -> ReceiverPaymentChannel
unsafeUpdateRecvState (CReceiverPaymentChannel s) (CPayment val sig) =
    CReceiverPaymentChannel $ s { pcsValueLeft = val, pcsPaymentSignature = Just sig}

