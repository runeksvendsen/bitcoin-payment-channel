
module Data.Bitcoin.PaymentChannel.Internal.Payment where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.State
import Data.Bitcoin.PaymentChannel.Internal.Script
import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Error

import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Internals as HI
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Script as HS
import Data.Word (Word32, Word64, Word8)
import Data.Maybe (fromJust, isJust)
import qualified Data.ByteString as B

---Payment Tx builer---
buildEmptyPaymentTx :: FundingTxInfo -> HT.Tx
buildEmptyPaymentTx (CFundingTxInfo hash idx _) =
    HT.Tx 1 --version 1
    -- Redeems payment channel output from blockchain Tx
    [HT.TxIn
        (HT.OutPoint hash idx)
        B.empty
        maxBound]
    [] --no outputs
    0 --lockTime 0

paymentTxAddOutput :: HT.TxOut -> HT.Tx -> (HT.Tx, HS.SigHash)
paymentTxAddOutput addOut tx@(HT.Tx _ _ outs _)
    | HT.outValue addOut >= fromIntegral dUST_LIMIT =
        (tx { HT.txOut = outs ++ [addOut] }, HS.SigSingle True)
    | otherwise =
        (tx, HS.SigNone True)
---Payment Tx builer---


getPaymentTxForSigning ::
    PaymentChannelState
    -> BitcoinAmount      -- ^New sender value (newValueLeft)
    -> (HT.Tx, HS.SigHash)
getPaymentTxForSigning st@(CPaymentChannelState _ fti
    (CPaymentTxConfig sendAddr) chanValLeft _) newValueLeft =
        paymentTxAddOutput senderOut $ buildEmptyPaymentTx fti
            where senderOut = HT.TxOut (toWord64 newValueLeft) (addressToScriptPubKeyBS sendAddr)

getPaymentTxHashForSigning ::
    PaymentChannelState
    -> BitcoinAmount -- ^New sender value (newValueLeft)
    -> (HC.Hash256, HS.SigHash)  -- ^Hash of payment transaction with a single output paying to senderPK
getPaymentTxHashForSigning st@(CPaymentChannelState
    cp@(CChannelParameters senderPK rcvrPK lt) _ _ _ _) newValueLeft =
        (HS.txSigHash tx (getRedeemScript cp) 0 sigHash, sigHash)
            where (tx,sigHash) = getPaymentTxForSigning st newValueLeft

---Payment build/verify---
verifyPayment ::
    PaymentChannelState
    -> Payment
    -> (HC.Hash256 -> HC.PubKey -> HC.Signature -> Bool)
    -> Bool
verifyPayment pcs
    (CPayment newSenderVal (CPaymentSignature sig sigHash)) verifyFunc =
        let (hash,_) = case sigHash of --SigHash in signature overrides newSenderVal
                HS.SigSingle True   -> getPaymentTxHashForSigning pcs newSenderVal
                --sender has relinquished remaining channel value
                HS.SigNone   True   -> getPaymentTxHashForSigning pcs 0
                unknownSigHash      -> (dummyHash256,sigHash) --will fail verification
        in
            verifyFunc hash (pcsClientPubKey pcs) sig

createPayment ::
    PaymentChannelState
    -> BitcoinAmount -- ^ newSenderVal
    -> (HC.Hash256 -> HC.Signature) -- ^ signing function
    -> Payment
createPayment pcs@(CPaymentChannelState _ _ _ currSenderVal _)
    newSenderVal signFunc =
    let
        (hash,sigHash) = getPaymentTxHashForSigning pcs newSenderVal
        sig = signFunc hash
        pSig = CPaymentSignature sig sigHash
    in
        case sigHash of
            HS.SigSingle    _ -> CPayment newSenderVal pSig
            --sender has reliquished the remaining channel value,
            -- due to it being below the "dust" limit.
            HS.SigNone      _ -> CPayment 0 pSig
            _                 -> error
                "BUG: unsupported SigHash created by 'getPaymentTxHashForSigning'"
---Payment build/verify---




