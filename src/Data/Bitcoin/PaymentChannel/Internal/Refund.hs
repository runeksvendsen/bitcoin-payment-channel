module Data.Bitcoin.PaymentChannel.Internal.Refund where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.State
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.Util

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


getRefundTxHashForSigning ::
    PaymentChannelState
    -> BitcoinAmount -- ^Bitcoin transaction fee
    -> HC.Hash256
getRefundTxHashForSigning pcs@(CPaymentChannelState _ cp _ _ _ _ _) newValueLeft =
        HS.txSigHash tx (getRedeemScript cp) 0 (HS.SigAll False)
            where tx = getUnsignedRefundTx pcs newValueLeft

refundTxAddSignature ::
    PaymentChannelState
    -> BitcoinAmount      -- ^Bitcoin tx fee
    -> HC.Signature     -- ^Signature over 'getUnsignedRefundTx' which verifies against clientPubKey
    -> FinalTx
refundTxAddSignature pcs@(CPaymentChannelState _ cp _ _ _ _ _) txFee clientRawSig =
        let
            inputScript = getP2SHInputScript cp $ refundTxScriptSig clientRawSig
        in
            replaceScriptInput 0 (serialize inputScript) $ getUnsignedRefundTx pcs txFee

