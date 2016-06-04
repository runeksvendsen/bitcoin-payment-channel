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

BitcoinLockTime, parseBitcoinLocktime, toWord32, fromDate, deserEither
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
    (PaymentChannelState(..), PaymentTxConfig(..))
-- import Data.Bitcoin.PaymentChannel.Internal.State
--     (ptcSenderChangeAddress)
import Data.Bitcoin.PaymentChannel.Internal.Script
    (getP2SHFundingAddress)
import Data.Bitcoin.PaymentChannel.Internal.Util
    (BitcoinLockTime, parseBitcoinLocktime, toWord32, fromDate, deserEither)
import Data.Bitcoin.PaymentChannel.Internal.State
    (setClientChangeAddress)

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




