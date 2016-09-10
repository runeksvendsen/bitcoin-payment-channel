module Data.Bitcoin.PaymentChannel.Internal.Settlement where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
import Data.Bitcoin.PaymentChannel.Internal.Util

import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Script as HS

-- |Sign everything, and do not allow additional inputs to be added afterwards.
serverSigHash = HS.SigAll False

csFromState :: PaymentChannelState -> ClientSignedPayment
csFromState cs@(CPaymentChannelState _ _ _ _ clientSig) =
    ClientSignedPayment (fromState cs) clientSig

toUnsignedSettlementTx :: ClientSignedPayment -> HC.Address -> BitcoinAmount -> HT.Tx
toUnsignedSettlementTx
        (ClientSignedPayment
            unsignedPayment@(UnsignedPayment _ valueAvailable _ _ senderVal)
        (CPaymentSignature _ sigHash) ) recvAddr txFee =
    let
        baseTx = toUnsignedBitcoinTx unsignedPayment
        adjustedTx = if sigHash == HS.SigNone True then removeOutputs baseTx else baseTx
        -- TODO: check dust?
        receiverAmount = valueAvailable - senderVal - txFee
        recvOut = HT.TxOut
                (fromIntegral . toInteger $ receiverAmount)
                (addressToScriptPubKeyBS recvAddr)
    in
        appendOutput adjustedTx recvOut

getSettlementTxHashForSigning ::
    ClientSignedPayment
    -> ChannelParameters
    -> HC.Address    -- ^Receiver destination address
    -> BitcoinAmount -- ^Bitcoin transaction fee
    -> HC.Hash256
getSettlementTxHashForSigning csPayment cp recvAddr txFee =
    HS.txSigHash tx (getRedeemScript cp) 0 serverSigHash
        where tx = toUnsignedSettlementTx csPayment recvAddr txFee

settlementSigningTxHashFromState ::
    PaymentChannelState
    -> HC.Address    -- ^Receiver destination address
    -> BitcoinAmount -- ^Bitcoin transaction fee
    -> HC.Hash256
settlementSigningTxHashFromState cs@(CPaymentChannelState cp _ _ _ _) =
    getSettlementTxHashForSigning (csFromState cs) cp

getSignedSettlementTx ::
    ClientSignedPayment
    -> ChannelParameters
    -> HC.Address       -- ^Receiver/server funds destination address
    -> BitcoinAmount    -- ^Bitcoin tx fee
    -> HC.Signature     -- ^Signature over 'getSettlementTxHashForSigning' which verifies against serverPubKey
    -> HT.Tx
getSignedSettlementTx csPayment@(ClientSignedPayment _ clientSig)
                      cp recvAddr txFee serverRawSig =
        let
            unsignedTx = toUnsignedSettlementTx csPayment recvAddr txFee
            serverSig = CPaymentSignature serverRawSig serverSigHash
            inputScript = getP2SHInputScript cp $ paymentTxScriptSig clientSig serverSig
        in
            replaceScriptInput (serialize inputScript) unsignedTx

signedSettlementTxFromState ::
    PaymentChannelState
    -> (HC.Hash256 -> HC.Signature) -- ^ Server/receiver's signing function. Produces a signature which verifies against 'cpReceiverPubKey'
    -> HC.Address       -- ^Receiver/server funds destination address
    -> BitcoinAmount    -- ^Bitcoin tx fee
    -> HT.Tx
signedSettlementTxFromState cs@(CPaymentChannelState cp _ _ _ _) signFunc recvAddr txFee =
    getSignedSettlementTx (csFromState cs) cp recvAddr txFee serverSig
        where serverSig = signFunc (settlementSigningTxHashFromState cs recvAddr txFee)
