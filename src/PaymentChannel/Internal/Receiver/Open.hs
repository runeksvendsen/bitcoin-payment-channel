{-# LANGUAGE DeriveAnyClass #-}
module PaymentChannel.Internal.Receiver.Open
( OpenError(..)
, initialServerState
)
where

import PaymentChannel.Internal.Util
import PaymentChannel.Internal.Receiver.Types
import PaymentChannel.RBPCP.Parse               (ParseError, fromPaymentData, parseRedeemScript)
import Bitcoin.SpendCond.Util                   (singlePrevIn, PickOutError)
import qualified RBPCP.Types                    as RBPCP
import qualified Network.Haskoin.Transaction    as HT


data OpenError
  = PickOutError (PickOutError ChanParams)    -- ^ Incorrect output specified
  | ResourcePaymentMismatch HT.Tx HT.TxHash   -- Redirect
  | FundingTxError ParseError
  | FundingTxDustOut BtcAmount
      deriving (Eq, Generic, NFData, ToJSON, FromJSON, Serialize)

-- | Derive the initial (zero-value) server state from
--    the funding transaction and "initial payment"-'PaymentData'
initialServerState ::
       ServerSettings
    -> HT.Tx
    -> RBPCP.PaymentData
    -> Either OpenError (ServerPayChanG () InvalidSig)
initialServerState cfg@ServerSettings{..} tx pd@RBPCP.PaymentData{..}
  | RBPCP.btcTxId paymentDataFundingTxid /= HT.txHash tx =
      Left $ ResourcePaymentMismatch tx (RBPCP.btcTxId paymentDataFundingTxid)
  | otherwise = do
      let rdmScrE = fmapL FundingTxError $ parseRedeemScript pd
          mkInput scr =  fmapL PickOutError (singlePrevIn tx scr paymentDataFundingVout)
      sp <- mkSignedPayment =<< mkInput =<< rdmScrE
      brandNewState cfg sp
    where
      mkSignedPayment input = fmapL FundingTxError $ runConfM cfg $
            fromPaymentData (btcInValue input) pd

brandNewState ::
       ServerSettings
    -> SignedPayment
    -> Either OpenError (ServerPayChanG () InvalidSig)
brandNewState cfg@ServerSettings{..} signedPaym = do
    let setErr = fmapL (const $ FundingTxDustOut serverConfDustLimit)
    newSp <- setErr $ runConfM cfg $ resetClientChangeVal signedPaym
    Right $ MkServerPayChan (mkChanState newSp) initialMetadata
  where
    mkChanState sp = MkPayChanState sp (fromInitialPayment signedPaym) cfg

instance Show OpenError where
    show (PickOutError e) = "Incorrect funding output specified: " ++ show e
    show (ResourcePaymentMismatch tx h) = unwords
        [ "prevOutHash mismatch for payment data/payment resource. resource: "
        , show (cs . encode . HT.txHash $ tx :: String)
        , "data:"
        , show (cs . encode $ h :: String)
        ]
    show (FundingTxError pe) = "funding tx error: " ++ show pe
    show (FundingTxDustOut dl) = unwords
        ["funding output below dust limit of", show dl]


