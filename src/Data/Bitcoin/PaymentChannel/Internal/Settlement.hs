module Data.Bitcoin.PaymentChannel.Internal.Settlement where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.State
import Data.Bitcoin.PaymentChannel.Internal.Script
import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Error

import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Internals as HI
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Script as HS


getSettlementTxForSigning ::
    PaymentChannelState
    -> HC.Address
    -> BitcoinAmount -- ^Bitcoin transaction fee for final payment transaction
    -> (HT.Tx, HS.SigHash) -- ^ Transaction plus valueReceiver SigHash
getSettlementTxForSigning st@(CPaymentChannelState _ fti@(CFundingTxInfo _ _ channelTotalValue)
    _ senderVal (Just (CPaymentSignature sig sigHash))) recvAddr txFee =
        let
            (baseTx,_) = getPaymentTxForSigning st senderVal
            adjTx = if sigHash == HS.SigNone True then removeOutputs baseTx else baseTx
            receiverAmount = channelTotalValue - senderVal - txFee -- may be less than zero
            recvOut = HT.TxOut (toWord64 receiverAmount) (addressToScriptPubKeyBS recvAddr)
        in
            paymentTxAddOutput recvOut adjTx
getSettlementTxForSigning _ _ _ = error "no payment sig available"

getSettlementTxHashForSigning ::
    PaymentChannelState
    -> HC.Address
    -> BitcoinAmount -- ^Bitcoin transaction fee
    -> HC.Hash256
getSettlementTxHashForSigning pcs@(CPaymentChannelState cp _ _ _ _) recvAddr txFee =
    HS.txSigHash tx (getRedeemScript cp) 0 sigHash
        where (tx,sigHash) = getSettlementTxForSigning pcs recvAddr txFee

getSignedSettlementTx ::
    PaymentChannelState
    -> HC.Address       -- ^Receiver/server change address
    -> BitcoinAmount    -- ^Bitcoin tx fee
    -> HC.Signature     -- ^Signature over 'getSettlementTxHashForSigning' which verifies against serverPubKey
    -> Either PayChanError FinalTx
getSignedSettlementTx pcs@(CPaymentChannelState
    cp@(CChannelParameters senderPK rcvrPK lt) _ _ _ (Just clientSig)) recvAddr txFee serverRawSig =
        let
            (tx,recvSigHash) = getSettlementTxForSigning pcs recvAddr txFee
            serverSig = CPaymentSignature serverRawSig recvSigHash
            inputScript = getInputScript cp $ paymentTxScriptSig clientSig serverSig
        in
            Right $ replaceScriptInput (serialize inputScript) tx
getSignedSettlementTx (CPaymentChannelState _ _ _ _ Nothing) _ _ _ = Left NoValueTransferred


