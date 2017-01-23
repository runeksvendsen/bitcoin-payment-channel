{-# LANGUAGE RecordWildCards, TypeSynonymInstances #-}

module PaymentChannel.Internal.State
(
  module PaymentChannel.Internal.State
, module PaymentChannel.Internal.Types
)
where

import PaymentChannel.Internal.Types
import PaymentChannel.Internal.Error
import PaymentChannel.Internal.Error.Status (checkReadyForPayment)
-- import PaymentChannel.Internal.Payment
import Bitcoin.Util  (addressToScriptPubKeyBS)
import PaymentChannel.Internal.ChanScript

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import           Data.Time.Clock
import qualified Data.Serialize     as Bin


-- ##########
-- ## Util ##
-- ##########
--
-- pcsFundingValue = ftiOutValue . pcsFundingTxInfo
-- pcsValueTransferred cs = nonDusty (pcsFundingValue cs) - nonDusty (pcsClientChangeVal cs)
-- pcsChannelValueLeft = pcsClientChangeVal
-- pcsClientPubKey = cpSenderPubKey . pcsParameters
-- pcsServerPubKey = cpReceiverPubKey . pcsParameters
-- pcsDustLimit = configDustLimit
-- pcsExpirationDate = cpLockTime . pcsParameters
-- pcsClientChangeAddress = ptcSenderChangeAddress . pcsPaymentConfig
-- pcsClientChangeScriptPubKey = addressToScriptPubKeyBS . pcsClientChangeAddress
-- pcsLockTime = cpLockTime . pcsParameters
-- pcsPrevOut (CPaymentChannelState _ (CFundingTxInfo h i _) _ _ _ _) = OutPoint h i
--
-- pcsFundingSource :: PaymentChannelState -> HT.OutPoint
-- pcsFundingSource pcs = HT.OutPoint (ftiHash fti) (ftiOutIndex fti)
--     where fti = pcsFundingTxInfo pcs
--
-- pcsGetPayment :: PaymentChannelState -> Payment
-- pcsGetPayment (CPaymentChannelState _ _ _ _ val sig) = CPayment val sig
--
-- -- |Set new client/sender change address.
-- -- Use this function if the client wishes to change its change address.
-- -- First set the new change address using this function, then accept the payment which
-- -- uses this new change address.
-- setClientChangeAddress :: PaymentChannelState -> HC.Address -> PaymentChannelState
-- setClientChangeAddress pcs@(CPaymentChannelState _ _ pConf _ _ _) addr =
--     pcs { pcsPaymentConfig = newPayConf }
--         where newPayConf = pConf { ptcSenderChangeAddress = addr }
--
-- setFundingSource :: PaymentChannelState -> FundingTxInfo -> PaymentChannelState
-- setFundingSource pcs fti =
--     pcs { pcsFundingTxInfo = fti }
--
-- -- |We subtract the specified "dust" limit from the total available value.
-- --  This avoids creating a Bitcoin transaction that won't circulate
-- --  in the Bitcoin P2P network.
-- channelValueLeft :: PaymentChannelState -> BtcAmount
-- channelValueLeft pcs =
--     nonDusty (pcsClientChangeVal pcs) - configDustLimit
--
-- -- |Returns 'True' if all available channel value has been transferred, 'False' otherwise
-- channelIsExhausted  :: PaymentChannelState -> Bool
-- channelIsExhausted pcs =
--     bsSigFlag (pcsPaymentSignature pcs) == HS.SigNone True ||
--         channelValueLeft pcs == 0

