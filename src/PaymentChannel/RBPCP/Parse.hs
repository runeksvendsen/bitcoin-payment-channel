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

import RBPCP.Types (PaymentData(..), JsonHex(..))
import qualified RBPCP.Types as RBPCP
import PaymentChannel.Internal.Util
import PaymentChannel.Internal.Types
import qualified Network.Haskoin.Transaction    as HT


data ParseError =
    BadRedeemScript String
  | BtcError BtcError
        deriving (Eq, Generic, NFData, ToJSON, FromJSON, Serialize)

toPaymentData :: Payment BtcSig -> PaymentData
toPaymentData (SigSinglePair input output) =
    PaymentData
        { paymentDataRedeemScript   = JsonHex . RBPCP.BtcScript . getRedeemScript . btcCondScr $ input
        , paymentDataFundingTxid    = RBPCP.BtcTxId . HT.outPointHash  . btcPrevOut $ input
        , paymentDataFundingVout    = HT.outPointIndex . btcPrevOut $ input
        , paymentDataSignatureData  = JsonHex .  bsSig . btcSigData $ input
        , paymentDataSighashFlag    = JsonHex .  bsSigFlag . btcSigData $ input
        , paymentDataChangeValue    = fromIntegral . nonDusty . btcAmount $ output
        , paymentDataChangeAddress  = btcAddress output
        }

fromPaymentData ::
    HasConfDustLimit m
    => BtcAmount
    -> PaymentData
    -> m (Either ParseError (Payment BtcSig))
fromPaymentData fundVal pd = do
    let inputE = paymentDataIn fundVal pd
    outputE <- paymentDataOut pd
    return $ do
        input  <- inputE
        output <- outputE
        Right $ SigSinglePair input output

paymentDataIn :: BtcAmount -> PaymentData -> Either ParseError (InputG P2SH ChanParams BtcSig)
paymentDataIn fundVal pd@PaymentData{..} =
    parseRedeemScript pd >>= Right . mapSigData (const paySig) . mkInput
  where
    mkInput r = mkNoSigTxIn prevOut fundVal r
    prevOut = HT.OutPoint (RBPCP.btcTxId paymentDataFundingTxid) paymentDataFundingVout
    -- TODO: strict DER/signature parsing
    paySig = BtcSig (fromHex paymentDataSignatureData)
                      (fromHex paymentDataSighashFlag)

parseRedeemScript :: PaymentData -> Either ParseError ChanParams
parseRedeemScript PaymentData{..} =
    fmapL BadRedeemScript $ fromRedeemScript (RBPCP.bsGetScript $ fromHex paymentDataRedeemScript)
  where

paymentDataOut :: HasConfDustLimit m => PaymentData -> m (Either ParseError BtcOut)
paymentDataOut PaymentData{..} =
    fmap (mkBtcOut paymentDataChangeAddress) .
        fmapL BtcError <$> mkNonDusty outAmount
  where
    outAmount = fromIntegral paymentDataChangeValue :: BtcAmount

instance Show ParseError where
    show (BadRedeemScript str) = "bad redeemScript: " ++ show str
    show (BtcError e) = "invalid Bitcoin transaction: " ++ show e
