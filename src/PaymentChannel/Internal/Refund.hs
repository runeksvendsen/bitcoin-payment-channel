module PaymentChannel.Internal.Refund where

import PaymentChannel.Internal.Settlement.Types
import PaymentChannel.Internal.Payment.Create
import Bitcoin.Signature

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC
import qualified Data.List.NonEmpty             as NE


type RefundTx = BtcTx P2SH ChanParams RefundScriptSig
type UnsignedRefundTx = UnsignedBtcTx P2SH ChanParams


mkBaseRefundTx :: ClientPayChan -> UnsignedRefundTx
mkBaseRefundTx cpc@MkClientPayChan{..} =
    let
        unsigPay = clearSig $ pcsPayment spcState
        cp = pairRedeemScript unsigPay
        unsigTx = toBtcTx unsigPay
        unsigTxIn = NE.head $ btcIns unsigTx
        -- If the sequence field equals maxBound (0xffffffff),
        --  lockTime features are disabled. so we subtract one from this
        refundIn  = setSequence (maxBound-1) unsigTxIn
        baseTx    = mkBtcTx (refundIn :| []) []
    in
        -- Setting the transaction lockTime allows it to redeem
        --  an output protected by OP_CHECKLOCKTIMEVERIFY. The
        --  transaction's lockTime must be greater than or equal to the lockTime
        --  passed to OP_CHECKLOCKTIMEVERIFY ('cpLockTime') in
        --  the script ('ChanParams').
        setLockTime (cpLockTime cp) baseTx

mkRefundTx
    :: ( Monad m, ToChangeOutFee fee )
    => ClientPayChan
    -> HC.Address                       -- ^Refund address
    -> fee                              -- ^Refund transaction fee
    -> m (Either BtcError RefundTx)     -- ^Refund Bitcoin transaction
mkRefundTx cpc@MkClientPayChan{..} refundAddr txFee = return $
    runSimple spcPrvKey $ signChangeTx refundTx changeOut
        where
            refundTx  = mkBaseRefundTx cpc
            changeOut = ChangeOut refundAddr (mkChangeFee txFee) KeepDust




