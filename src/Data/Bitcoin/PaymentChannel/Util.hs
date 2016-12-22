{-|
Module      : Data.Bitcoin.PaymentChannel.Types
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com

Utility functions for "Data.Bitcoin.PaymentChannel".

-}

module Data.Bitcoin.PaymentChannel.Util
(
getFundingAddress,Scr.getRedeemScript,
setSenderChangeAddress,

serialize, deserEither,
BitcoinLockTime, parseBitcoinLocktime, toWord32, fromDate,
Ser.parseJSONWord,
pGetSig,fpGetSig,

unsafeUpdateRecvState
)
where

import Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
import Data.Bitcoin.PaymentChannel.Internal.State                           (setClientChangeAddress)
import qualified Data.Bitcoin.PaymentChannel.Internal.Serialization.JSON    as Ser
import qualified Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script        as Scr

import Data.Bitcoin.PaymentChannel.Types
--     (ChannelParameters, FundingTxInfo)

import qualified Network.Haskoin.Crypto as HC

-- | Derive a Bitcoin address, for funding a payment channel, from
--  'ChannelParameters'.
--  The transaction which pays to this address is the channel funding transaction,
--  and information about this transaction is contained in
--  'FundingTxInfo'.
getFundingAddress :: ChannelParameters -> HC.Address
getFundingAddress = Scr.getP2SHFundingAddress

-- |Set new value sender change address
setSenderChangeAddress :: PaymentChannel a => a -> HC.Address -> a
setSenderChangeAddress pch addr =
    _setChannelState pch (setClientChangeAddress (getChannelState pch) addr)

-- |Update internal state without signature verification.
-- Useful for database-type services where a logic layer has already
--  verified the signature, and it just needs to be stored.
unsafeUpdateRecvState :: ReceiverPaymentChannelI a -> Payment -> ReceiverPaymentChannelI a
unsafeUpdateRecvState (CReceiverPaymentChannel s pki) (CPayment val sig) =
    CReceiverPaymentChannel ( s { pcsClientChangeVal = val, pcsPaymentSignature = sig} ) pki

fpGetSig :: FullPayment -> HC.Signature
fpGetSig = psSig . paySignature . fpPayment

pGetSig :: Payment -> HC.Signature
pGetSig = psSig . paySignature
