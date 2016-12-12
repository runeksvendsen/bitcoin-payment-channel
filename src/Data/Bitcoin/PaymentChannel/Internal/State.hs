{-# LANGUAGE RecordWildCards #-}

module Data.Bitcoin.PaymentChannel.Internal.State where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Error
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.Util  (addressToScriptPubKeyBS)
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import           Data.Time.Clock


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

-- |Update state with payment containing a valid signature.
updatePaymentChannelState
    :: PaymentChannelState
    -> FullPayment
    -> UTCTime
    -> Either PayChanError PaymentChannelState
updatePaymentChannelState pcs paym now =
    checkFundingSource paym pcs >>=
    checkChangeAddr paym >>=
    checkRedeemScript paym >>=
    checkDustOutput paym >>=
    checkExpirationTime now >>=
    updatePayCount paym >>=
    updateValue paym >>=
    updateSignature paym

updateSignature :: FullPayment -> PayChanState -> Either PayChanError PayChanState
updateSignature CFullPayment{..} pcs
    | not $ verifyPaymentSigFromState pcs checkSigFunc fpPayment = Left SigVerifyFailed
    | otherwise = Right pcs
        { pcsPaymentSignature = paySignature fpPayment }
    where checkSigFunc hash pk sig = HC.verifySig hash sig (getPubKey pk)

updateValue :: FullPayment -> PayChanState -> Either PayChanError PayChanState
updateValue CFullPayment{..} pcs@CPaymentChannelState{..}
    | CPayment newSenderVal _ <- fpPayment =
        if newSenderVal > pcsClientChangeVal then
            Left $ BadPaymentValue (newSenderVal - pcsClientChangeVal)
        else
            Right pcs { pcsClientChangeVal = newSenderVal }

checkFundingSource :: FullPayment -> PayChanState -> Either PayChanError PayChanState
checkFundingSource CFullPayment{..} pcs@CPaymentChannelState{..}
    | HT.OutPoint payHash payIdx <- fpOutPoint
    , CFundingTxInfo h i _       <- pcsFundingTxInfo
    , payHash /= h || payIdx /= i =
        Left $ OutPointMismatch $ OutPoint h i
    | otherwise = Right pcs

checkChangeAddr :: FullPayment -> PayChanState -> Either PayChanError PayChanState
checkChangeAddr CFullPayment{..} pcs@CPaymentChannelState{..}
    | fpChangeAddr /= pcsClientChangeAddress pcs =
        Left $ ChangeAddrMismatch $ pcsClientChangeAddress pcs
    | otherwise = Right pcs

checkRedeemScript :: FullPayment -> PayChanState -> Either PayChanError PayChanState
checkRedeemScript CFullPayment{..} pcs@CPaymentChannelState{..}
    | fpChanParams /= pcsParameters =
        Left $ RedeemScriptMismatch $ getRedeemScript pcsParameters
    | otherwise = Right pcs

checkDustOutput :: FullPayment -> PayChanState -> Either PayChanError PayChanState
checkDustOutput CFullPayment{..} pcs@CPaymentChannelState{..}
    | Config dustLimit _ <- pcsConfig
    , payClientChange fpPayment < dustLimit =
        Left $ DustOutput dustLimit
    | otherwise = Right pcs

checkExpirationTime :: UTCTime -> PayChanState -> Either PayChanError PayChanState
checkExpirationTime now pcs@CPaymentChannelState{..}
    | isPastLockTimeDate now pcsConfig pcsParameters =
        Left ChannelExpired
    | otherwise =
        Right pcs

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
    -- in order to make sure we don't accept block count-based locktimes for now.

updatePayCount :: FullPayment -> PayChanState -> Either PayChanError PayChanState
updatePayCount CFullPayment{..} pcs@CPaymentChannelState{..}
    | payClientChange fpPayment == pcsClientChangeVal =
        Right pcs
    | otherwise = Right pcs { pcsPaymentCount = pcsPaymentCount+1 }

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

-- |Create a 'ReceiverPaymentChannelX', which has an associated XPubKey, from a
--  'ReceiverPaymentChannel'
mkExtendedKeyRPC :: ReceiverPaymentChannel -> HC.XPubKey -> Maybe ReceiverPaymentChannelX
mkExtendedKeyRPC (CReceiverPaymentChannel pcs _) xpk =
    -- Check that it's the right pubkey first
    if xPubKey xpk == getPubKey (pcsServerPubKey pcs) then
            Just $ CReceiverPaymentChannel pcs $
                Metadata (HC.xPubIndex xpk) (pcsValueTransferred pcs) ReadyForPayment
        else
            Nothing

markAsBusy :: ReceiverPaymentChannelX -> ReceiverPaymentChannelX
markAsBusy pcs@CReceiverPaymentChannel{ rpcMetadata = meta } =
    pcs { rpcMetadata = metaSetBusy meta }

isChannelBusy :: ReceiverPaymentChannelX -> Bool
isChannelBusy CReceiverPaymentChannel{..} =
    metaIsBusy rpcMetadata

class UpdateMetadata a where
    calcNewData :: a -> PaymentChannelState -> a

instance UpdateMetadata () where
    calcNewData _ _ = ()

instance UpdateMetadata Metadata where
    calcNewData (Metadata ki oldValRecvd s) pcs =
        Metadata ki checkedVal s
        where checkedVal  = if newValRecvd < oldValRecvd then error "BUG: Value lost :(" else newValRecvd
              newValRecvd = pcsValueTransferred pcs

updateWithMetadata :: UpdateMetadata d => d -> PaymentChannelState -> ReceiverPaymentChannelI d
updateWithMetadata oldData pcs =
    CReceiverPaymentChannel pcs (calcNewData oldData pcs)

