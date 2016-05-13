-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Data.Bitcoin.PaymentChannel.Internal.State where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Error

import qualified Network.Haskoin.Transaction as HT

pcsChannelTotalValue = ftiOutValue . pcsFundingTxInfo
pcsValueTransferred cs = pcsChannelTotalValue cs - pcsValueLeft cs
pcsChannelValueLeft = pcsValueLeft
pcsClientPubKey = cpSenderPubKey . pcsParameters
pcsServerPubKey = cpReceiverPubKey . pcsParameters
pcsClientChange = ptcSenderChangeScript . pcsPaymentConfig
pcsServerChange = ptcReceiverChangeScript . pcsPaymentConfig
pcsLockTime = cpLockTime . pcsParameters
pcsChannelID = ftiHash . pcsFundingTxInfo


-- pcsChannelIsExhausted cs = maybe False (\pSig -> psSigHash pSig == ) (pcsPaymentSignature cs)

newPaymentChannelState channelParameters fundingTxInfo paymentConfig =
    CPaymentChannelState {
        pcsParameters           = channelParameters,
        pcsFundingTxInfo        = fundingTxInfo,
        pcsPaymentConfig        = paymentConfig,
        pcsValueLeft            = ftiOutValue fundingTxInfo,
        pcsPaymentSignature     = Nothing
    }

-- |Update state with verified payment
updatePaymentChannelState  ::
    PaymentChannelState
    -> Payment
    -> Either PayChanError PaymentChannelState
updatePaymentChannelState pcs@(CPaymentChannelState par fun pconf oldSenderVal oldSig)
    (CPayment newSenderVal newSig)
        | newSenderVal <= oldSenderVal =
            Right $ CPaymentChannelState par fun pconf newSenderVal (Just newSig)
        | otherwise = Left BadPayment
