{-# LANGUAGE DeriveGeneric #-}
module Bitcoin.SinglePair
( module Bitcoin.SinglePair
, module X
)
where

import Bitcoin.Types            as X
import Bitcoin.LockTime.Util    as X
import Bitcoin.Signature        as X
import Bitcoin.SpendCond.Cond   as X
import Bitcoin.Tx
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC
import qualified Data.List.NonEmpty     as NE
import Control.Monad.Time
{-# ANN module ("HLint: ignore Use isNothing"::String) #-}


mkSigSinglePair :: InputG t r () -> BtcOut -> SigSinglePair t r ()
mkSigSinglePair pin out =
    SigSinglePair (adjustSignFlag pin) out
  where
    adjustSignFlag = setSignFlag $
        if btcAmount out /= nullAmount
            then HS.SigSingle True
            else HS.SigNone   True

signPair :: ( Monad m, Show r
            , TransformSigData BtcSig () r --, SignatureScript t r BtcSig
            , SpendFulfillment BtcSig r
            ) =>
       HC.PrvKeyC
    -> SigSinglePair t r ()
    -> m (Either BtcError (SigSinglePair t r BtcSig))
signPair prvKey sp@SigSinglePair{..} =
    return $ fromBtcTx <$> runSimple prvKey (signTx $ toBtcTx sp)

singlePairVerifySig ::
   ( Show ss, Show t
   , SpendFulfillment ss r
   , SpendCondition r
   ) =>
   SigSinglePair t r ss -> Either VerifyError ()
singlePairVerifySig sp = verifyTx (toBtcTx sp)

-- | Won't work until SegWit
toClientSignedTxSegWit :: Show r => NE.NonEmpty (SigSinglePair t r BtcSig) -> BtcTx t r BtcSig
toClientSignedTxSegWit spL = foldr addInOut (toBtcTx $ NE.last spL) (NE.init spL)
    where addInOut SigSinglePair{..} tx@BtcTx{..} =
             tx { btcIns  = singleInput NE.<| btcIns
                , btcOuts = singleOutput : btcOuts
                }


getSigData :: SigSinglePair t r sd -> sd
getSigData = btcSigData . singleInput

pairRedeemScript :: SigSinglePair t r a -> r
pairRedeemScript SigSinglePair{..} = btcCondScr singleInput

fundingValue :: SigSinglePair t r a -> BtcAmount
fundingValue SigSinglePair{..} = btcInValue singleInput

clientChangeVal :: SigSinglePair t r a -> BtcAmount
clientChangeVal SigSinglePair{..} = nonDusty (btcAmount singleOutput)

resetClientChangeVal ::
       HasConfDustLimit m
    => SigSinglePair t r BtcSig
    -> m (Either BtcError (SigSinglePair t r InvalidSig))
resetClientChangeVal ssp@SigSinglePair{..} =
    fmap (mapSigData fromBtcSig . mkNew) <$> mkNonDusty (fundingValue ssp)
  where
    mkNew fundVal =
        ssp { singleOutput =
                 singleOutput { btcAmount = fundVal }
            }


clientChangeAddr :: SigSinglePair t r a -> HC.Address
clientChangeAddr SigSinglePair{..} = btcAddress singleOutput

class SetClientChangeAddr (s :: * -> *) where
    _setClientChangeAddr :: s BtcSig -> HC.Address -> s InvalidSig

instance SetClientChangeAddr (SigSinglePair t r) where
    _setClientChangeAddr ssp@SigSinglePair{..} addr =
        mapSigData fromBtcSig $ ssp { singleOutput =
                 singleOutput { btcAddress = addr }
            }

clearSig :: SigSinglePair t r a -> SigSinglePair t r ()
clearSig = mapSigData $ const ()


-- | After the lockTime -- specified in the funding output in the blockchain --
--    has passed, the client/sender can redeem all sent funds. So we make sure
--    this isn't possible before accepting a payment.
fundingIsLocked ::
    ( Show r
    , Show sd
    , MonadTime m
    , HasLockTimeDate r
    )
    => Seconds
    -> SigSinglePair t r sd
    -> m Bool
fundingIsLocked settlePeriodSeconds =
    allInputsLocked settlePeriodSeconds . toBtcTx
