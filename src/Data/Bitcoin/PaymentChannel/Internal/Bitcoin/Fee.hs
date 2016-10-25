{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Fee where

import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount
import qualified Data.Serialize     as Bin


-- |Objects from which a Bitcoin fee can be calculated,
--   given a transaction (yeah you need to create a tx twice).
class HasFee a where
    absoluteFee :: TxByteSize -> a -> BitcoinAmount

-- |For compatibility
instance HasFee BitcoinAmount where
    absoluteFee _ = id    -- Same as constant fee

data Constant = Constant BitcoinAmount
instance HasFee Constant where
    absoluteFee _ (Constant amt) = amt

type TxByteSize = Word
-- |Specify a fee as satoshis per byte
newtype SatoshisPerByte = SatoshisPerByte BitcoinAmount -- ^Fee in satoshis per byte
    deriving (Eq, Show, Bin.Serialize, Ord, Num, Enum, Real, Integral)
instance HasFee SatoshisPerByte where
    absoluteFee txByteSize (SatoshisPerByte satoshisPerByte) =
        fromIntegral txByteSize * satoshisPerByte

