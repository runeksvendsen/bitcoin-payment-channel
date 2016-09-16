-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Data.Bitcoin.PaymentChannel.Internal.State where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Error
import Data.Bitcoin.PaymentChannel.Internal.Util  (addressToScriptPubKeyBS)
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS

pcsChannelTotalValue = ftiOutValue . pcsFundingTxInfo
pcsValueTransferred cs = pcsChannelTotalValue cs - pcsValueLeft cs
pcsChannelValueLeft = pcsValueLeft
pcsClientPubKey = cpSenderPubKey . pcsParameters
pcsServerPubKey = cpReceiverPubKey . pcsParameters
pcsDustLimit = cpDustLimit . pcsParameters
pcsExpirationDate = cpLockTime . pcsParameters
pcsClientChangeAddress = ptcSenderChangeAddress . pcsPaymentConfig
pcsClientChangeScriptPubKey = addressToScriptPubKeyBS . pcsClientChangeAddress
pcsLockTime = cpLockTime . pcsParameters

pcsChannelID :: PaymentChannelState -> HT.OutPoint
pcsChannelID pcs = HT.OutPoint (ftiHash fti) (ftiOutIndex fti)
    where fti = pcsFundingTxInfo pcs

pcsGetPayment :: PaymentChannelState -> Payment
pcsGetPayment (CPaymentChannelState _ _ _ val sig) = CPayment val sig

-- |Set new client/sender change address.
-- Use this function if the client wishes to change its change address.
-- First set the new change address using this function, then accept the payment which
-- uses this new change address.
setClientChangeAddress :: PaymentChannelState -> HC.Address -> PaymentChannelState
setClientChangeAddress pcs@(CPaymentChannelState _ _ pConf _ _) addr =
    pcs { pcsPaymentConfig = newPayConf }
        where newPayConf = pConf { ptcSenderChangeAddress = addr }

-- |We subtract the specified "dust" limit from the total available value,
--  in order to avoid creating a Bitcoin transaction that won't circulate
--  in the Bitcoin P2P network.
channelValueLeft :: PaymentChannelState -> BitcoinAmount
channelValueLeft pcs@(CPaymentChannelState (CChannelParameters _ _ _ dustLimit) _ _ _ _)   =
    pcsValueLeft pcs - dustLimit

-- |Returns 'True' if all available channel value has been transferred, 'False' otherwise
channelIsExhausted  :: PaymentChannelState -> Bool
channelIsExhausted pcs =
    psSigHash (pcsPaymentSignature pcs) == HS.SigNone True ||
        channelValueLeft pcs == 0

newPaymentChannelState channelParameters fundingTxInfo paymentConfig paySig =
    CPaymentChannelState {
        pcsParameters           = channelParameters,
        pcsFundingTxInfo        = fundingTxInfo,
        pcsPaymentConfig        = paymentConfig,
        pcsValueLeft            = ftiOutValue fundingTxInfo,
        pcsPaymentSignature     = paySig
    }

-- |Update state with verified payment.
updatePaymentChannelState  ::
    PaymentChannelState
    -> FullPayment
    -> Either PayChanError PaymentChannelState
updatePaymentChannelState (CPaymentChannelState cp fun@(CFundingTxInfo h i _)
                          pconf@(CPaymentTxConfig addr) oldSenderVal _)
    (CFullPayment payment@(CPayment newSenderVal _) payOP payScript payChgAddr)
        | (HT.outPointHash payOP /= h) || (HT.outPointIndex payOP /= i) =
            Left $ OutPointMismatch $ OutPoint h i
        | payChgAddr /= addr =
            Left $ ChangeAddrMismatch addr
        | payScript /= getRedeemScript cp =
            Left $ RedeemScriptMismatch $ getRedeemScript cp
        | newSenderVal <= oldSenderVal =
            CPaymentChannelState cp fun pconf newSenderVal . cpSignature <$>
                checkDustLimit cp payment
        | otherwise = Left $ BadPaymentValue (newSenderVal - oldSenderVal)

checkDustLimit :: ChannelParameters -> Payment -> Either PayChanError Payment
checkDustLimit (CChannelParameters _ _ _ dustLimit) payment@(CPayment senderChangeVal _)
    | senderChangeVal < dustLimit =
        Left $ DustOutput dustLimit
    | otherwise = Right payment
