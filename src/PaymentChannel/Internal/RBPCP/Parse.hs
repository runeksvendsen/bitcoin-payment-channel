module PaymentChannel.Internal.RBPCP.Parse
(
  fromPaymentData
, toPaymentData
)
where

import RBPCP.Types
import PaymentChannel.Internal.Util
import PaymentChannel.Internal.Types
import Bitcoin.SpendCond.Cond
-- import PaymentChannel.Internal.Payment.Types ()
import qualified Network.Haskoin.Transaction    as HT
import qualified Data.List.NonEmpty             as NE


data ParseError =
    BadRedeemScript String
  | BtcError BtcError

fromPaymentData :: BtcAmount -> PaymentData -> Either ParseError (SigSinglePair ChanParams BtcSig)
fromPaymentData fundVal pd = do
    input  <- paymentDataIn fundVal pd
    output <- paymentDataOut pd
    Right $ SigSinglePair input output

paymentDataIn :: BtcAmount -> PaymentData -> Either ParseError (InputG ChanParams BtcSig)
paymentDataIn fundVal PaymentData{..} =
    chanParamsE >>= Right . mapSigData (const paySig) . mkInput
  where
    mkInput = mkNoSigTxIn prevOut fundVal
    prevOut = HT.OutPoint paymentDataFundingTxid paymentDataFundingVout
    chanParamsE = fmapL BadRedeemScript $ fromRedeemScript (fromHex paymentDataRedeemScript)
    -- TODO: strict DER/signature parsing
    paySig = MkBtcSig (fromHex paymentDataSignatureData)
                      (fromHex paymentDataSighashFlag)

paymentDataOut :: PaymentData -> Either ParseError BtcOut
paymentDataOut PaymentData{..} =
    mkBtcOut paymentDataChangeAddress <$>
        fmapL BtcError (mkNonDusty outAmount)
  where
    outAmount = fromIntegral paymentDataChangeValue :: BtcAmount

toPaymentData :: SigSinglePair (P2SH ChanParams) BtcSig -> PaymentData
toPaymentData (SigSinglePair input output) =
    PaymentData
        { paymentDataRedeemScript   = JsonHex . getRedeemScript . getCond . btcInType $ input
        , paymentDataFundingTxid    = HT.outPointHash  . btcPrevOut $ input
        , paymentDataFundingVout    = HT.outPointIndex . btcPrevOut $ input
        , paymentDataSignatureData  = JsonHex .  bsSig . btcSigData $ input
        , paymentDataSighashFlag    = JsonHex .  bsSigFlag . btcSigData $ input
        , paymentDataChangeValue    = fromIntegral . nonDusty . btcAmount $ output
        , paymentDataChangeAddress  = btcAddress output
        }


