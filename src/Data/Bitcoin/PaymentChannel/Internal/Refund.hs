module Data.Bitcoin.PaymentChannel.Internal.Refund where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.State
import Data.Bitcoin.PaymentChannel.Internal.Script
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.Util

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS

getUnsignedRefundTx :: PaymentChannelState -> BitcoinAmount -> HT.Tx
getUnsignedRefundTx st txFee = -- @CPaymentChannelState =
    let
        (baseTx,_) = getPaymentTxForSigning st 0 --create empty payment tx, which redeems funding tx
        refundOut = HT.TxOut (toWord64 $ pcsChannelTotalValue st - txFee) (pcsClientChange st)
    in
        baseTx { HT.txOut = [refundOut] }

getRefundTxHashForSigning ::
    PaymentChannelState
    -> BitcoinAmount -- ^Bitcoin transaction fee
    -> HC.Hash256
getRefundTxHashForSigning pcs@(CPaymentChannelState cp _ _ _ _) newValueLeft =
        HS.txSigHash tx (getRedeemScript cp) 0 (HS.SigAll False)
            where tx = getUnsignedRefundTx pcs newValueLeft

refundTxAddSignature ::
    PaymentChannelState
    -> BitcoinAmount      -- ^Bitcoin tx fee
    -> HC.Signature     -- ^Signature over 'getUnsignedRefundTx' which verifies against clientPubKey
    -> FinalTx
refundTxAddSignature pcs@(CPaymentChannelState cp _ _ _ _) txFee clientRawSig =
        let
            inputScript = getInputScript cp $ refundTxScriptSig clientRawSig
        in
            replaceScriptInput (serialize inputScript) $ getUnsignedRefundTx pcs txFee

