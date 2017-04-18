{-# LANGUAGE DeriveAnyClass #-}
module PaymentChannel.Internal.Receiver.Open
( OpenError(..)
, initialServerState
)
where

import PaymentChannel.Internal.Util
import PaymentChannel.Internal.Receiver.Types
import PaymentChannel.RBPCP.Parse               (ParseError, fromPaymentData, parseRedeemScript)
import Bitcoin.SpendCond.Util                   (singlePrevIn)
import qualified RBPCP.Types                    as RBPCP
import qualified Network.Haskoin.Transaction    as HT



data OpenError
  = NoSuchOutput Word32 HT.TxHash
  | ResourcePaymentMismatch HT.Tx HT.TxHash   -- Redirect
  | IrrelevantOutput Word32 HT.TxHash ChanParams
  | FundingTxError ParseError
  | FundingTxDustOut BtcAmount
      deriving (Eq, Generic, NFData, ToJSON, FromJSON, Serialize)

-- | Derive the initial (zero-value) server state from
--    the funding transaction and "initial payment"-'PaymentData'
initialServerState :: HT.Tx -> RBPCP.PaymentData -> Either OpenError (ServerPayChanG () InvalidSig)
initialServerState tx pd@RBPCP.PaymentData{..}
  | i <- paymentDataFundingVout
  , indexMax <- length (HT.txOut tx) - 1
  , i > fromIntegral indexMax =
      Left $ NoSuchOutput i (HT.txHash tx)
  | paymentDataFundingTxid /= HT.txHash tx =
      Left $ ResourcePaymentMismatch tx paymentDataFundingTxid
  | otherwise = do
      rdmScr <- fmapL FundingTxError $ parseRedeemScript pd
      case singlePrevIn tx rdmScr paymentDataFundingVout of
          Nothing  -> Left $
              IrrelevantOutput paymentDataFundingVout (HT.txHash tx) rdmScr
          Just inp ->
                fmapL FundingTxError (fromPaymentData (btcInValue inp) pd)
                  >>= brandNewState

brandNewState :: SignedPayment -> Either OpenError (ServerPayChanG () InvalidSig)
brandNewState signedPaym = do
    let setErr = fmapL (const $ FundingTxDustOut configDustLimit)
    newSp <- setErr $ resetClientChangeVal signedPaym
    return $ MkServerPayChan (mkChanState newSp) initialMetadata
  where
    mkChanState sp = MkPayChanState sp (fromInitialPayment signedPaym)

instance Show OpenError where
    show (NoSuchOutput i h) = unwords
        [ "no such output:"
        , showOut h i
        ]
    show (ResourcePaymentMismatch tx h) = unwords
        [ "prevOutHash mismatch for payment data/payment resource. resource: "
        , show (cs . encode . HT.txHash $ tx :: String)
        , "data:"
        , show (cs . encode $ h :: String)
        ]
    show (IrrelevantOutput i h scr) = unwords
        [ showOut h i
        , "doesn't pay to redeemScript"
        , cs . encode $ scr
        ]
    show (FundingTxError pe) = "funding tx error: " ++ show pe
    show (FundingTxDustOut dl) = unwords
        ["funding output below dust limit of", show dl]


showOut h i = show i ++ ":" ++ cs (encode h)
