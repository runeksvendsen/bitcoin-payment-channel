{-|
Module      : PaymentChannel.Types
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com

Utility functions for "PaymentChannel".

-}

module PaymentChannel.Util
(
getFundingAddress,Scr.getRedeemScript,

serialize, deserEither,
BtcLockTime, parseLockTime, toWord32, fromDate,
Ser.parseJSONWord,
)
where

import           Bitcoin.Util
import qualified PaymentChannel.Internal.ChanScript         as Scr
import           PaymentChannel.Internal.Receiver.Types
import qualified PaymentChannel.Internal.Serialization.JSON as Ser

import           PaymentChannel.Types
--     (ChanParams, FundingTxInfo)

import qualified Network.Haskoin.Crypto                     as HC

-- | Derive a Bitcoin address, for funding a payment channel, from
--  'ChanParams'.
--  The transaction which pays to this address is the channel funding transaction,
--  and information about this transaction is contained in
--  'FundingTxInfo'.
getFundingAddress :: ChanParams -> HC.Address
getFundingAddress = Scr.getP2SHFundingAddress
