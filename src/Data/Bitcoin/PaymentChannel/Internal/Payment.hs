
module Data.Bitcoin.PaymentChannel.Internal.Payment where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
import Data.Bitcoin.PaymentChannel.Internal.Util

import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Script as HS
import qualified Data.ByteString as B


-- |Represents a P2SH ANYONECANPAY input/output pair from a payment channel.
--  This input/output pair can be added to any transaction, as long as the
--  input and corresponding output are at the same index number
--  (and tx version and lockTime match as well).
data UnsignedPayment = UnsignedPayment
  {  fundingOutPoint    :: HT.OutPoint
  ,  outPointValue      :: BitcoinAmount
  ,  redeemScript       :: HS.Script
  ,  changeAddress      :: HC.Address
  ,  changeValue        :: BitcoinAmount
  }

fromState :: PaymentChannelState -> UnsignedPayment
fromState (CPaymentChannelState cp (CFundingTxInfo hash idx val)
          (CPaymentTxConfig changeAddr) changeValue _) =
    UnsignedPayment (HT.OutPoint hash idx) val (getRedeemScript cp) changeAddr changeValue

toUnsignedBitcoinTx :: UnsignedPayment -> HT.Tx
toUnsignedBitcoinTx (UnsignedPayment fundingOutPoint _ _ changeAddr changeVal) =
    HT.Tx
        1 --version 1
        -- Redeems payment channel output from blockchain Tx
        [HT.TxIn
            fundingOutPoint
            B.empty
            maxBound]
        [senderOut] -- change output
        0 --lockTime 0
    where senderOut = HT.TxOut
            (fromIntegral . toInteger $ changeVal)
            (addressToScriptPubKeyBS changeAddr)

getHashForSigning :: UnsignedPayment -> HS.SigHash -> HC.Hash256
getHashForSigning up@(UnsignedPayment _ _ redeemScript _ _) =
    HS.txSigHash
        (toUnsignedBitcoinTx up)
        redeemScript
        0 -- In the tx we construct using 'toUnsignedBitcoinTx',
          --  the input in question is always at index 0

---Payment create/verify---
-- |Create payment, from state. No check of value is performed.
paymentFromState ::
    PaymentChannelState
    -> BitcoinAmount                -- ^ sender change value (subtract 'n' from current sender change value to get payment of value 'n')
    -> (HC.Hash256 -> HC.Signature) -- ^ signing function
    -> Payment
paymentFromState (CPaymentChannelState cp fti (CPaymentTxConfig changeAddr) _ _) =
    createPayment cp fti changeAddr

-- |Create payment, pure.
createPayment ::
    ChannelParameters
    -> FundingTxInfo
    -> HC.Address
    -> BitcoinAmount -- ^ sender change value (subtract 'n' from current sender change value to get payment of value 'n')
    -> (HC.Hash256 -> HC.Signature) -- ^ signing function
    -> Payment
createPayment cp (CFundingTxInfo hash idx val) changeAddr changeVal signFunc =
    let
        payProxy = UnsignedPayment
                (HT.OutPoint hash idx) val (getRedeemScript cp)
                changeAddr changeVal
        sigHash = if changeVal /= 0 then HS.SigSingle True else HS.SigNone True
        hashToSign = getHashForSigning payProxy sigHash
        sig = signFunc hashToSign
    in
        CPayment changeVal (CPaymentSignature sig sigHash)

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
            payProxy = UnsignedPayment
                    (HT.OutPoint fundTxId idx) val (getRedeemScript cp)
                     changeAddr newSenderVal
            hash = case sigHash of
                HS.SigSingle True   -> getHashForSigning payProxy sigHash
                --sender has relinquished remaining channel value
                HS.SigNone   True   -> getHashForSigning payProxy sigHash
                _                   -> dummyHash256 --will fail verification
        in
            verifyFunc hash sendPK sig

verifyPaymentSigFromState ::
    PaymentChannelState
    -> (HC.Hash256 -> SendPubKey -> HC.Signature -> Bool)
    -> Payment
    -> Bool
verifyPaymentSigFromState (CPaymentChannelState cp fti (CPaymentTxConfig changeAddr) _ _) =
    verifyPaymentSig cp fti changeAddr (cpSenderPubKey cp)
---Payment create/verify---




