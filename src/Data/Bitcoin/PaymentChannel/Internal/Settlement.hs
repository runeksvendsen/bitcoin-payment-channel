module Data.Bitcoin.PaymentChannel.Internal.Settlement
(
    createSignedSettlementTx
,   signedSettlementTxFromState
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Fee

import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Script as HS

-- |Sign everything, and do not allow additional inputs to be added afterwards.
serverSigHash = HS.SigAll False

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

getSettlementTxHashForSigning
    :: ClientSignedPayment
    -> ChannelParameters
    -> HC.Address    -- ^Receiver destination address
    -> BitcoinAmount           -- ^Bitcoin transaction fee
    -> HC.Hash256
getSettlementTxHashForSigning csPayment cp recvAddr txFee =
    HS.txSigHash tx (getRedeemScript cp) 0 serverSigHash
        where tx = toUnsignedSettlementTx csPayment recvAddr txFee

getSignedSettlementTx
    :: ClientSignedPayment
    -> ChannelParameters
    -> HC.Address       -- ^Receiver/server funds destination address
    -> (HC.Hash256 -> HC.Signature) -- ^ Server/receiver's signing function. Produces a signature which verifies against 'cpReceiverPubKey'
    -> BitcoinAmount    -- ^Bitcoin tx fee
    -> HT.Tx
getSignedSettlementTx csPayment@(ClientSignedPayment _ clientSig)
                      cp recvAddr signFunc txFee =
        let
            rawServerSig = signFunc $ getSettlementTxHashForSigning csPayment cp recvAddr txFee
            unsignedTx = toUnsignedSettlementTx csPayment recvAddr txFee
            serverSig = CPaymentSignature rawServerSig serverSigHash
            inputScript = getP2SHInputScript cp $ paymentTxScriptSig clientSig serverSig
        in
            replaceScriptInput 0 (serialize inputScript) unsignedTx

createSignedSettlementTx
    :: HasFee fee
    => ClientSignedPayment
    -> ChannelParameters
    -> HC.Address                   -- ^Receiver/server funds destination address
    -> (HC.Hash256 -> HC.Signature) -- ^Server/receiver's signing function. Produces a signature which verifies against 'cpReceiverPubKey'
    -> fee                          -- ^Bitcoin tx fee
    -> HT.Tx
createSignedSettlementTx csp cp addr signFunc fee =
    getSignedSettlementTx csp cp addr signFunc (absoluteFee (calcTxSize zeroFeeTx) fee)
    where
        zeroFeeTx = getSignedSettlementTx csp cp addr signFunc (0 :: BitcoinAmount)

signedSettlementTxFromState
    :: HasFee fee
    => PaymentChannelState
    -> HC.Address                   -- ^Receiver/server funds destination address
    -> (HC.Hash256 -> HC.Signature) -- ^ Server/receiver's signing function. Produces a signature which verifies against 'cpReceiverPubKey'
    -> fee                          -- ^Bitcoin tx fee
    -> HT.Tx
signedSettlementTxFromState cs@(CPaymentChannelState _ cp _ _ _ _ _) =
    createSignedSettlementTx (cPaymentFromState cs) cp

cPaymentFromState :: PaymentChannelState -> ClientSignedPayment
cPaymentFromState cs@(CPaymentChannelState _ _ _ _ _ _ clientSig) =
    ClientSignedPayment (fromState cs) clientSig
