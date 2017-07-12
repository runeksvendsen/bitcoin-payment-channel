{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bitcoin.Fee where

import           Bitcoin.Amount
import qualified Data.Aeson.Types             as JSON
import qualified Data.Serialize               as Bin
import           PaymentChannel.Internal.Util

-- |Objects from which a Bitcoin fee can be calculated,
--   given a transaction
class HasFee a where
    absoluteFee
        :: BtcAmount    -- ^ Available value (max fee) (use 'maxBound' if you don't know)
        -> TxByteSize   -- ^ Transaction size in bytes
        -> a            -- ^ Fee-like type
        -> BtcAmount    -- ^ Absolute fee

-- |Constant fee
instance HasFee BtcAmount where
    absoluteFee _ _ = id    -- Same as constant fee

newtype Constant = Constant BtcAmount
    deriving (Eq, Generic, NFData)
instance HasFee Constant where
    absoluteFee _ _ (Constant amt) = amt

type TxByteSize = Word
-- |Specify a fee as satoshis per byte
newtype SatoshisPerByte = SatoshisPerByte BtcAmount -- ^Fee in satoshis per byte
    deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bin.Serialize, JSON.ToJSON, JSON.FromJSON, NFData)
instance HasFee SatoshisPerByte where
    absoluteFee _ txByteSize (SatoshisPerByte satoshisPerByte) =
        fromIntegral txByteSize * satoshisPerByte

-- | A maximum fee of two fees. Whichever fee results in the largest absolute fee is chosen.
newtype MaxFee a b = MaxFee (a,b)
    deriving (Eq, Show, Ord, Bin.Serialize, JSON.ToJSON, JSON.FromJSON, NFData)
instance (HasFee a, HasFee b) => HasFee (MaxFee a b) where
    absoluteFee availVal txByteSize (MaxFee (f1,f2)) =
        max (getAbsFee f1) (getAbsFee f2)
      where
        getAbsFee :: HasFee fee => fee -> BtcAmount
        getAbsFee = absoluteFee availVal txByteSize

-- | At most the specified fee. Will not fail if the specified amount is more than is available.
newtype Capped val = Capped val
instance HasFee fee => HasFee (Capped fee) where
    absoluteFee availVal size (Capped fee) =
        min availVal desiredFee
      where
        desiredFee = absoluteFee availVal size fee



{-
mkRelativeFeeTx
    :: HasFee fee
    => fee                          -- ^Desired (per-byte) transaction fee
    -> (BtcAmount -> HT.Tx)     -- ^Produces desired Bitcoin tx with given fee
    -> HT.Tx
mkRelativeFeeTx fee mkTxFunc =
    mkTxSizeFee pass2Tx
    where
        pass2Tx = mkTxSizeFee pass1Tx
        pass1Tx = mkTxFunc (0 :: BtcAmount)
        mkTxSizeFee tx = mkTxFunc $ absoluteFee (calcTxSize tx) fee
-}
