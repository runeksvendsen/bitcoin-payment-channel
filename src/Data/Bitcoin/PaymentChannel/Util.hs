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

BitcoinLockTime, parseBitcoinLocktime, toWord32, fromDate, deserEither
)
where

import Data.Bitcoin.PaymentChannel.Internal.Script
    (getP2SHFundingAddress)
import Data.Bitcoin.PaymentChannel.Internal.Util
    (BitcoinLockTime, parseBitcoinLocktime, toWord32, fromDate, deserEither)

-- Only used to make Haddock display the right link
import Data.Bitcoin.PaymentChannel.Types
    (ChannelParameters, FundingTxInfo)

-- | Derive a Bitcoin address, for funding a payment channel, from
--  'ChannelParameters'.
--  The transaction which pays to this address is the channel funding transaction,
--  and information about this transaction is contained in
--  'FundingTxInfo'.
getFundingAddress = getP2SHFundingAddress