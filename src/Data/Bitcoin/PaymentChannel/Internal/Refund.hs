module Data.Bitcoin.PaymentChannel.Internal.Refund where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.State
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Fee

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS


getUnsignedRefundTx :: PaymentChannelState -> BitcoinAmount -> HT.Tx
getUnsignedRefundTx st txFee =
    let
        baseTx = toUnsignedBitcoinTx $ fromState st --create empty payment tx, which redeems funding tx
        refundOut = HT.TxOut
                (fromIntegral . toInteger $ pcsChannelTotalValue st - txFee)
                (pcsClientChangeScriptPubKey st)
        txInput0 = head $ HT.txIn baseTx
    in
        HT.createTx
            (HT.txVersion baseTx)
            -- if the sequence field equals maxBound (0xffffffff)
            -- lockTime features are disabled, so we subtract one
            [ txInput0 { HT.txInSequence = maxBound-1 } ]
            [refundOut]
            -- lockTime of refund tx must be greater than or equal to the lockTime
            -- in the channel redeemScript
            (toWord32 $ pcsLockTime st)


getRefundTxHashForSigning
    :: PaymentChannelState
    -> BitcoinAmount -- ^Bitcoin transaction fee
    -> HC.Hash256
getRefundTxHashForSigning pcs@(CPaymentChannelState _ cp _ _ _ _ _) txFee =
        HS.txSigHash tx (getRedeemScript cp) 0 (HS.SigAll False)
            where tx = getUnsignedRefundTx pcs txFee

refundTxCreate ::
    PaymentChannelState
    -> BitcoinAmount                -- ^Bitcoin tx fee
    -> (HC.Hash256 -> HC.Signature) -- ^Produces a signature that verifies against clientPubKey
    -> HT.Tx
refundTxCreate pcs@(CPaymentChannelState _ cp _ _ _ _ _) txFee signFunc =
        let
            inputScript = getP2SHInputScript cp $ refundTxScriptSig sig
            sig = signFunc $ getRefundTxHashForSigning pcs txFee
        in
            replaceScriptInput 0 (serialize inputScript) $ getUnsignedRefundTx pcs txFee


mkRefundTx :: HasFee fee => PaymentChannelState -> fee -> (HC.Hash256 -> HC.Signature) -> HT.Tx
mkRefundTx pcs txFee signFunc =
    refundTxCreate pcs (absoluteFee (calcTxSize zeroFeeTx) txFee) signFunc
    where
        zeroFeeTx = refundTxCreate pcs (0 :: BitcoinAmount) signFunc


