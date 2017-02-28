{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module PaymentChannel.RBPCP.Parse
(
  fromPaymentData
, toPaymentData
, RBPCP.PaymentData
, ParseError(..)
)
where

import RBPCP.Types
import qualified RBPCP.Types as RBPCP
import PaymentChannel.Internal.Util
import PaymentChannel.Internal.Types
import Bitcoin.SpendCond.Cond
-- import PaymentChannel.Internal.Payment.Types ()
import qualified Network.Haskoin.Transaction    as HT
import qualified Data.List.NonEmpty             as NE
import PaymentChannel.Internal.Crypto.PubKey


data ParseError =
    BadRedeemScript String
  | BtcError BtcError
        deriving (Eq, Generic, NFData, ToJSON, FromJSON, Serialize)

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

fromPaymentData :: BtcAmount -> PaymentData -> Either ParseError (SigSinglePair (P2SH ChanParams) BtcSig)
fromPaymentData fundVal pd = do
    input  <- paymentDataIn fundVal pd
    output <- paymentDataOut pd
    Right $ SigSinglePair input output

paymentDataIn :: BtcAmount -> PaymentData -> Either ParseError (InputG (P2SH ChanParams) BtcSig)
paymentDataIn fundVal PaymentData{..} =
    chanParamsE >>= Right . mapSigData (const paySig) . mkInput
  where
    mkInput r = mkNoSigTxIn prevOut fundVal (Pay2 (ScriptHash (Cond r)))
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

instance Show ParseError where
    show (BadRedeemScript str) = "bad redeemScript: " ++ show str
    show (BtcError e) = "Invalid Bitcoin transaction: " ++ show e
