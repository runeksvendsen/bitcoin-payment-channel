-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Data.Bitcoin.PaymentChannel.Internal.State where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Error
import Data.Bitcoin.PaymentChannel.Internal.Util  (addressToScriptPubKeyBS, BitcoinAmount)

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

pcsChannelID :: PaymentChannelState -> HT.OutPoint
pcsChannelID pcs = HT.OutPoint (ftiHash fti) (ftiOutIndex fti)
    where fti = pcsFundingTxInfo pcs

-- |Set new client/sender change address.
-- Use this function if the client wishes to change its change address.
-- First set the new change address using this function, then accept the payment which
-- uses this new change address.
setClientChangeAddress :: PaymentChannelState -> HC.Address -> PaymentChannelState
setClientChangeAddress pcs@(CPaymentChannelState _ _ pConf _ _) addr =
    pcs { pcsPaymentConfig = newPayConf }
        where newPayConf = pConf { ptcSenderChangeAddress = addr }

-- |
channelValueLeft :: PaymentChannelState -> BitcoinAmount
channelValueLeft pcs   =
    if channelIsExhausted pcs then 0 else pcsValueLeft pcs

-- |Returns 'True' if all available channel value has been transferred, 'False' otherwise
channelIsExhausted  :: PaymentChannelState -> Bool
channelIsExhausted pcs =
    case pcsPaymentSignature pcs of
        Nothing -> False
        -- Channel can be auto-closed when sender has given up all value
        -- which requires a SigNone signature
        Just paySig -> psSigHash paySig == HS.SigNone True

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
        | otherwise = Left $ BadPaymentValue (newSenderVal - oldSenderVal)

checkDustLimit :: Payment -> Either PayChanError Payment
checkDustLimit payment@(CPayment senderChangeVal sig)
    | senderChangeVal < dUST_LIMIT =
        if psSigHash sig /= HS.SigNone True then Left DustOutput else Right payment
    | otherwise = Right payment