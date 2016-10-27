-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Data.Bitcoin.PaymentChannel.Internal.State where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Error
import Data.Bitcoin.PaymentChannel.Internal.Util  (addressToScriptPubKeyBS)
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import           Data.Time.Clock


pcsChannelTotalValue = ftiOutValue . pcsFundingTxInfo
pcsValueTransferred cs = pcsChannelTotalValue cs - pcsClientChangeVal cs
pcsChannelValueLeft = pcsClientChangeVal
pcsClientPubKey = cpSenderPubKey . pcsParameters
pcsServerPubKey = cpReceiverPubKey . pcsParameters
pcsDustLimit = cDustLimit . pcsConfig
pcsExpirationDate = cpLockTime . pcsParameters
pcsClientChangeAddress = ptcSenderChangeAddress . pcsPaymentConfig
pcsClientChangeScriptPubKey = addressToScriptPubKeyBS . pcsClientChangeAddress
pcsLockTime = cpLockTime . pcsParameters
pcsPrevOut (CPaymentChannelState _ _ (CFundingTxInfo h i _) _ _ _ _) = OutPoint h i

pcsChannelFundingSource :: PaymentChannelState -> HT.OutPoint
pcsChannelFundingSource pcs = HT.OutPoint (ftiHash fti) (ftiOutIndex fti)
    where fti = pcsFundingTxInfo pcs

pcsGetPayment :: PaymentChannelState -> Payment
pcsGetPayment (CPaymentChannelState _ _ _ _ _ val sig) = CPayment val sig

-- |Set new client/sender change address.
-- Use this function if the client wishes to change its change address.
-- First set the new change address using this function, then accept the payment which
-- uses this new change address.
setClientChangeAddress :: PaymentChannelState -> HC.Address -> PaymentChannelState
setClientChangeAddress pcs@(CPaymentChannelState _ _ _ pConf _ _ _) addr =
    pcs { pcsPaymentConfig = newPayConf }
        where newPayConf = pConf { ptcSenderChangeAddress = addr }

setFundingSource :: PaymentChannelState -> FundingTxInfo -> PaymentChannelState
setFundingSource pcs fti =
    pcs { pcsFundingTxInfo = fti }

-- |We subtract the specified "dust" limit from the total available value.
--  This avoids creating a Bitcoin transaction that won't circulate
--  in the Bitcoin P2P network.
channelValueLeft :: PaymentChannelState -> BitcoinAmount
channelValueLeft pcs@(CPaymentChannelState (Config dustLimit _) _ _ _ _ _ _)   =
    pcsClientChangeVal pcs - dustLimit

-- |Returns 'True' if all available channel value has been transferred, 'False' otherwise
channelIsExhausted  :: PaymentChannelState -> Bool
channelIsExhausted pcs =
    psSigHash (pcsPaymentSignature pcs) == HS.SigNone True ||
        channelValueLeft pcs == 0

newPaymentChannelState cfg channelParameters fundingTxInfo paymentConfig paySig =
    CPaymentChannelState {
        pcsConfig               = cfg,
        pcsParameters           = channelParameters,
        pcsFundingTxInfo        = fundingTxInfo,
        pcsPaymentConfig        = paymentConfig,
        pcsPaymentCount         = 0,
        pcsClientChangeVal      = ftiOutValue fundingTxInfo,
        pcsPaymentSignature     = paySig
    }


-- |Update state with verified payment.
updatePaymentChannelState  ::
    PaymentChannelState
    -> FullPayment
    -> Either PayChanError PaymentChannelState
updatePaymentChannelState (CPaymentChannelState cfg cp fun@(CFundingTxInfo h i _)
                          pconf@(CPaymentTxConfig addr) payCount oldSenderVal _)
    (CFullPayment payment@(CPayment newSenderVal _) payOP payScript payChgAddr)
        | (HT.outPointHash payOP /= h) || (HT.outPointIndex payOP /= i) =
            Left $ OutPointMismatch $ OutPoint h i
        | payChgAddr /= addr =
            Left $ ChangeAddrMismatch addr
        | payScript /= getRedeemScript cp =
            Left $ RedeemScriptMismatch $ getRedeemScript cp
        | newSenderVal > oldSenderVal =
            Left $ BadPaymentValue (newSenderVal - oldSenderVal)
        | otherwise =
            CPaymentChannelState cfg cp fun pconf (payCount+1) newSenderVal . cpSignature <$>
                checkDustLimit cfg payment

-- |Create a 'ReceiverPaymentChannelX', which has an associated XPubKey, from a
--  'ReceiverPaymentChannel'
mkExtendedKeyRPC :: ReceiverPaymentChannel -> HC.XPubKey -> Maybe ReceiverPaymentChannelX
mkExtendedKeyRPC (CReceiverPaymentChannel pcs _) xpk =
    -- Check that it's the right pubkey first
    if xPubKey xpk == getPubKey (pcsServerPubKey pcs) then
            Just $ CReceiverPaymentChannel pcs (HC.xPubIndex xpk)
        else
            Nothing

checkDustLimit :: Config -> Payment -> Either PayChanError Payment
checkDustLimit (Config dustLimit _) payment@(CPayment senderChangeVal _)
    | senderChangeVal < dustLimit =
        Left $ DustOutput dustLimit
    | otherwise = Right payment

isPastLockTimeDate :: UTCTime
                   -> Config
                   -> ChannelParameters
                   -> Bool
isPastLockTimeDate currentTime (Config _ settlePeriodHrs) (CChannelParameters _ _ (LockTimeDate expTime)) =
    currentTime > (settlePeriod `addUTCTime` expTime)
        where settlePeriod = -1 * fromIntegral (toSeconds settlePeriodHrs) :: NominalDiffTime
isPastLockTimeDate _ _ (CChannelParameters _ _ (LockTimeBlockHeight _)) =
    True
    -- We don't have the current Bitcoin block so we regard the channel as expired,
    -- in order to make we don't accept block count-based locktimes for now.

