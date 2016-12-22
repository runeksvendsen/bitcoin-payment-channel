{-# LANGUAGE RecordWildCards #-}
module Data.Bitcoin.PaymentChannel.Internal.Payment.Payment where

import Data.Bitcoin.PaymentChannel.Internal.Payment.Types
import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
import Data.Bitcoin.PaymentChannel.Internal.Util

import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Script as HS
import qualified Data.ByteString as B


-- |scriptSig fulfilling 'mkChannelRedeemScript' using two signatures (client+server)
paymentTxScriptSig :: PaymentSignature -> PaymentSignature -> Script --ScriptSig
paymentTxScriptSig clientSig serverSig = Script
    [opPushData $ serialize clientSig, --sig including SigHash byte
    opPushData $ serialize serverSig,  --sig including SigHash byte
    OP_1]   -- Make reeemScript OP_IF evaluate to true (two pubkeys/sigs)


toUnsignedInMaybeOut :: Show kd => ClientSignedPaymentI kd -> (HT.TxIn, Maybe HT.TxOut)
toUnsignedInMaybeOut csp =
    let (input, output) = toClientUnsignedInOut $ cspPayment csp in
    case psSigHash (clientSig csp) of
        HS.SigNone   _ -> (input, Nothing)
        HS.SigSingle _ -> (input, Just output)
        x              -> error $ "Bad SigHash in ClientSignedPayment: " ++ show (x, csp)

cPaymentFromState :: PaymentChannelState -> ClientSignedPayment
cPaymentFromState cs@CPaymentChannelState{..} =
    ClientSignedPayment (fromState cs) pcsPaymentSignature ()

cspRedeemScript :: ClientSignedPaymentI kd -> Script
cspRedeemScript = getRedeemScript . upChannelParams . cspPayment


cspReplaceKeyData :: b -> ClientSignedPaymentI a -> ClientSignedPaymentI b
cspReplaceKeyData new csp = csp { csKeyData = new }

cspReplaceKeyDataM :: Monad m => (a -> m b) -> ClientSignedPaymentI a -> m (ClientSignedPaymentI b)
cspReplaceKeyDataM f csp = do
    newKD <- f $ csKeyData csp
    return $ cspReplaceKeyData newKD csp


fromState :: PaymentChannelState -> UnsignedPayment
fromState (CPaymentChannelState _ cp (CFundingTxInfo hash idx fundingVal)
          (CPaymentTxConfig changeAddr) _ changeValue _) =
    UnsignedPayment
        (HT.OutPoint hash idx) fundingVal cp changeAddr changeValue


toClientUnsignedInOut :: UnsignedPayment -> (HT.TxIn, HT.TxOut)
toClientUnsignedInOut UnsignedPayment{..} = (in', out)
    where
        -- Redeems funding output from in-blockchain Tx
        in' = HT.TxIn fundingOutPoint B.empty maxBound
        out = HT.TxOut -- client change output
            (fromIntegral . toInteger $ changeValue)
            (addressToScriptPubKeyBS changeAddress)

toUnsignedBitcoinTx :: UnsignedPayment -> HT.Tx
toUnsignedBitcoinTx up =
    let (input, output) = toClientUnsignedInOut up in
        HT.createTx
            1 --version 1
            [input]
            [output]
            0 --lockTime 0

getHashForSigning :: UnsignedPayment -> HS.SigHash -> HC.Hash256
getHashForSigning up@(UnsignedPayment _ _ cp _ _) =
    HS.txSigHash
        (toUnsignedBitcoinTx up)
        (getRedeemScript cp)
        0 -- In the tx we construct using 'toUnsignedBitcoinTx',
          --  the input in question is always at index 0

---Payment create/verify---
-- |Create payment, from state. No check of value is performed.
mkPaymentFromState ::
    PaymentChannelState
    -> BitcoinAmount                -- ^ sender change value (subtract 'n' from current sender change value to get payment of value 'n')
    -> (HC.Hash256 -> HC.Signature) -- ^ signing function
    -> FullPayment
mkPaymentFromState (CPaymentChannelState _ cp fti (CPaymentTxConfig changeAddr) _ _ _) =
    createPayment cp fti changeAddr

-- |Create payment, pure.
createPayment ::
    ChannelParameters
    -> FundingTxInfo
    -> HC.Address
    -> BitcoinAmount -- ^ sender change value (subtract 'n' from current sender change value to get payment of value 'n')
    -> (HC.Hash256 -> HC.Signature) -- ^ signing function
    -> FullPayment
createPayment cp (CFundingTxInfo hash idx val) changeAddr changeVal signFunc =
    let
        unsignedPayment@(UnsignedPayment op _ _ _ _) = UnsignedPayment
                (HT.OutPoint hash idx) val cp
                changeAddr changeVal
        sigHash = if changeVal /= 0 then HS.SigSingle True else HS.SigNone True
        hashToSign = getHashForSigning unsignedPayment sigHash
        sig = signFunc hashToSign
        payment = CPayment changeVal (CPaymentSignature sig sigHash)
    in
        CFullPayment payment op cp changeAddr


verifyPaymentSig ::
    ChannelParameters
    -> FundingTxInfo
    -> HC.Address
    -> SendPubKey
    -> (HC.Hash256 -> SendPubKey -> HC.Signature -> Bool)
    -> Payment
    -> Bool
verifyPaymentSig cp (CFundingTxInfo fundTxId idx val) changeAddr sendPK verifyFunc
    (CPayment newSenderVal (CPaymentSignature sig sigHash)) =
        let
            payProxy = UnsignedPayment (HT.OutPoint fundTxId idx) val cp
                            changeAddr newSenderVal
            hash = case sigHash of
                HS.SigSingle True   -> getHashForSigning payProxy sigHash
                --sender has relinquished remaining channel value
                HS.SigNone   True   -> getHashForSigning payProxy sigHash
                _                   -> dummyHash256 --will fail verification. probably.
        in
            verifyFunc hash sendPK sig

verifyPaymentSigFromState ::
    PaymentChannelState
    -> (HC.Hash256 -> SendPubKey -> HC.Signature -> Bool)
    -> Payment
    -> Bool
verifyPaymentSigFromState (CPaymentChannelState _ cp fti (CPaymentTxConfig changeAddr) _ _ _) =
    verifyPaymentSig cp fti changeAddr (cpSenderPubKey cp)
---Payment create/verify---




