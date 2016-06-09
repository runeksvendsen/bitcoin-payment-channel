-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Data.Bitcoin.PaymentChannel.Internal.State where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Error
import Data.Bitcoin.PaymentChannel.Internal.Util  (addressToScriptPubKeyBS)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS

pcsChannelTotalValue = ftiOutValue . pcsFundingTxInfo
pcsValueTransferred cs = pcsChannelTotalValue cs - pcsValueLeft cs
pcsChannelValueLeft = pcsValueLeft
pcsClientPubKey = cpSenderPubKey . pcsParameters
pcsServerPubKey = cpReceiverPubKey . pcsParameters
pcsClientChangeAddress = ptcSenderChangeAddress . pcsPaymentConfig
pcsClientChange = addressToScriptPubKeyBS . pcsClientChangeAddress
pcsLockTime = cpLockTime . pcsParameters
pcsChannelID = ftiHash . pcsFundingTxInfo

-- |Set new client/sender change address.
-- Use this function if the client wishes to change its change address.
-- First set the new change address using this function, then accept the payment which
-- uses this new change address.
setClientChangeAddress :: PaymentChannelState -> HC.Address -> PaymentChannelState
setClientChangeAddress pcs@(CPaymentChannelState _ _ pConf _ _) addr =
    pcs { pcsPaymentConfig = newPayConf }
        where newPayConf = pConf { ptcSenderChangeAddress = addr }


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
    payment@(CPayment newSenderVal newSig)
        | newSenderVal <= oldSenderVal =
            fmap (Just . cpSignature) (checkDustLimit payment) >>=
                return . (CPaymentChannelState par fun pconf newSenderVal) -- (Just newSig)
        | otherwise = Left BadPaymentValue

checkDustLimit :: Payment -> Either PayChanError Payment
checkDustLimit payment@(CPayment senderChangeVal sig)
    | senderChangeVal < dUST_LIMIT =
        if psSigHash sig /= HS.SigNone True then Left DustOutput else Right payment
    | otherwise = Right payment