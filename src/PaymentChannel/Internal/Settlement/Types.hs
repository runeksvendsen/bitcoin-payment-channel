{-# LANGUAGE FlexibleContexts, DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module PaymentChannel.Internal.Settlement.Types
(
  module PaymentChannel.Internal.Settlement.Types
, module PaymentChannel.Internal.Payment.Types
, module PaymentChannel.Internal.Settlement.Config
)
where

import PaymentChannel.Internal.Settlement.Script
import Bitcoin.Util
import PaymentChannel.Internal.Payment.Types
import PaymentChannel.Internal.Settlement.Config

import qualified Network.Haskoin.Crypto             as HC
import qualified Network.Haskoin.Script             as HS
import           Data.Word                          (Word64)
import           Data.Time.Clock                    (UTCTime)
{-# ANN module ("HLint: ignore Use mapMaybe"::String) #-}



data RefundScriptSig = RefundScriptSig
   { rssClientSig   :: BtcSig
   }


instance HasLockTimeDate (P2SH ChanParams) where
    getLockTimeDate = cpLockTime . getCond

instance SpendFulfillment RefundScriptSig ChanParams where
    rawSigs RefundScriptSig{..} MkChanParams{..} =
        [ (getPubKey cpReceiverPubKey, rssClientSig) ]
    signatureScript RefundScriptSig{..} _ = Script
        [ opPush rssClientSig
        , OP_0 ] -- Make reeemScript OP_IF evaluate to false.
                 -- Signal that we want to provide only one pubkey/sig pair (sender's),
                 -- after it is checked that the lockTime has expired.



-- |After a settlement transaction is published, we save
--   certain properties from the old state object before
--   it's replaced with a new one.
data SettleInfo = SettleInfo
    { siValue       :: BtcAmount
    , siPayCount    :: Word64
    } deriving (Eq, Show, Generic, Serialize, FromJSON, ToJSON)



type ClientSignedTx = BtcTx (P2SH ChanParams) BtcSig
