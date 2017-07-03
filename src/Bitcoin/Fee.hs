{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bitcoin.Fee where

-- import Bitcoin.Types
import           Bitcoin.Amount
import qualified Data.Aeson.Types             as JSON
import qualified Data.Serialize               as Bin
import           PaymentChannel.Internal.Util

-- |Objects from which a Bitcoin fee can be calculated,
--   given a transaction
class HasFee a where
    absoluteFee :: BtcAmount -> TxByteSize -> a -> BtcAmount

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
