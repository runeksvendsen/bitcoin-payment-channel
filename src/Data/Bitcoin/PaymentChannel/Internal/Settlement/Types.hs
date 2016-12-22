{-# LANGUAGE FlexibleContexts, DeriveAnyClass #-}
module Data.Bitcoin.PaymentChannel.Internal.Settlement.Types
(
  module Data.Bitcoin.PaymentChannel.Internal.Settlement.Types
, module Data.Bitcoin.PaymentChannel.Internal.Payment.Types
-- , module Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
)
where

import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Payment.Types


import qualified Network.Haskoin.Crypto             as HC
import           Data.Word                          (Word64)
import           Data.Time.Clock                    (UTCTime)
{-# ANN module ("HLint: ignore Use mapMaybe"::String) #-}


-- |Contains necessary data to produce server signature.
data UnsignedSettlementTx kd = UnsignedSettlementTx
    { sspClientPayments     :: [ClientSignedPaymentI kd]        -- ^ One or more client payments. >1 not supported until SegWit is enabled.
    , sspAdditionalOuts     :: [(HC.Address, BitcoinAmount)]    -- ^ Additional receiver destination addresses+amounts
    , sspReceiverAddress    :: HC.Address                       -- ^ Server change Bitcoin address
    , sspTxFee              :: BitcoinAmount                    -- ^ Bitcoin transaction fee
    } deriving (Eq, Show)

type UnsignedSettlementTxIdx = UnsignedSettlementTx KeyDeriveIndex
type UnsignedSettlementTxPrv = UnsignedSettlementTx HC.PrvKeyC


-- |After a settlement transaction is published, we save
--   certain properties from the old state object before
--   it's replaced with a new one.
data SettleInfo = SettleInfo
    { siValue       :: BitcoinAmount
--     , siTime        :: UTCTime
    , siPayCount    :: Word64
    } deriving (Eq, Show, Generic, Serialize, FromJSON, ToJSON)
