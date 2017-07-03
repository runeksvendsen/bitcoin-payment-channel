module PaymentChannel.Internal.Refund where

import PaymentChannel.Internal.Settlement.Types
import PaymentChannel.Internal.Payment.Create
import Bitcoin.Signature

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC



type RefundTx = BtcTx P2SH ChanParams RefundScriptSig
type UnsignedRefundTx = UnsignedBtcTx P2SH ChanParams


-- | Returns Nothing if there's not enough value available to cover paying the specified fee
--    without producing a dust output.
mkBaseRefundTx :: ChanParams -> FundingTxInfo -> UnsignedRefundTx
mkBaseRefundTx cp CFundingTxInfo{..} =
    let
        baseIn = setSignFlag (HS.SigAll False) $ mkNoSigTxIn
                             (HT.OutPoint ftiHash ftiOutIndex)
                             (nonDusty ftiOutValue)
                             cp
        -- If the sequence field equals maxBound (0xffffffff),
        --  lockTime features are disabled. so we subtract one
        refundIn  = setSequence (maxBound-1) baseIn
        baseTx    = mkBtcTx (refundIn :| []) []
    in
        setLockTime (cpLockTime cp) baseTx

mkRefundTx
    :: Monad m
    => HC.PrvKeyC
    -> ChanParams
    -> FundingTxInfo
    -> HC.Address                       -- ^Refund address
    -> SatoshisPerByte                  -- ^Refund transaction fee
    -> m (Either BtcError RefundTx)     -- ^Refund Bitcoin transaction
mkRefundTx prvKey cp fti refundAddr txFee = return $
    runSimple prvKey $ signChangeTx refundTx changeOut
        where
            refundTx  = mkBaseRefundTx cp fti
            changeOut = mkChangeOut refundAddr txFee KeepDust
--            signFunc _ = return prvKey




