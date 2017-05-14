{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module PaymentChannel.RBPCP.Parse
(
  fromPaymentData
, toPaymentData
, RBPCP.PaymentData(..)
, ParseError(..)
, parseRedeemScript
)
where

import RBPCP.Types
import qualified RBPCP.Types as RBPCP
import PaymentChannel.Internal.Util
import PaymentChannel.Internal.Types
import qualified Network.Haskoin.Transaction    as HT


data ParseError =
    BadRedeemScript String
  | BtcError BtcError
        deriving (Eq, Generic, NFData, ToJSON, FromJSON, Serialize)

toPaymentData :: SigSinglePair ScriptType BtcSig -> PaymentData
toPaymentData (SigSinglePair input output) =
    PaymentData
        { paymentDataRedeemScript   = JsonHex . getRedeemScript . getCond . btcInType $ input
        , paymentDataFundingTxid    = RBPCP.BtcTxId . HT.outPointHash  . btcPrevOut $ input
        , paymentDataFundingVout    = HT.outPointIndex . btcPrevOut $ input
        , paymentDataSignatureData  = JsonHex .  bsSig . btcSigData $ input
        , paymentDataSighashFlag    = JsonHex .  bsSigFlag . btcSigData $ input
        , paymentDataChangeValue    = fromIntegral . nonDusty . btcAmount $ output
        , paymentDataChangeAddress  = btcAddress output
        }

fromPaymentData :: BtcAmount -> PaymentData -> Either ParseError (SigSinglePair ScriptType BtcSig)
fromPaymentData fundVal pd = do
    input  <- paymentDataIn fundVal pd
    output <- paymentDataOut pd
    Right $ SigSinglePair input output

paymentDataIn :: BtcAmount -> PaymentData -> Either ParseError (InputG ScriptType BtcSig)
paymentDataIn fundVal pd@PaymentData{..} =
    parseRedeemScript pd >>= Right . mapSigData (const paySig) . mkInput
  where
    mkInput r = mkNoSigTxIn prevOut fundVal (Pay2 (ScriptHash (Cond r)))
    prevOut = HT.OutPoint (RBPCP.btcTxId paymentDataFundingTxid) paymentDataFundingVout
    -- TODO: strict DER/signature parsing
    paySig = MkBtcSig (fromHex paymentDataSignatureData)
                      (fromHex paymentDataSighashFlag)

parseRedeemScript :: PaymentData -> Either ParseError ChanParams
parseRedeemScript PaymentData{..} =
    fmapL BadRedeemScript $ fromRedeemScript (fromHex paymentDataRedeemScript)

paymentDataOut :: PaymentData -> Either ParseError BtcOut
paymentDataOut PaymentData{..} =
    mkBtcOut paymentDataChangeAddress <$>
        fmapL BtcError (mkNonDusty outAmount)
  where
    outAmount = fromIntegral paymentDataChangeValue :: BtcAmount

instance Show ParseError where
    show (BadRedeemScript str) = "bad redeemScript: " ++ show str
    show (BtcError e) = "invalid Bitcoin transaction: " ++ show e
