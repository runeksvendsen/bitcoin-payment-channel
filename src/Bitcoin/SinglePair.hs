{-# LANGUAGE DeriveGeneric #-}
module Bitcoin.SinglePair
(
  module Bitcoin.SinglePair
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


instance (Show r, Show sd) => IsTxLike SigSinglePair r sd where
    toBtcTx SigSinglePair{..} =
        BtcTx 1 inputL [singleOutput] Nothing Nothing
            where inputL  = singleInput NE.:| []
    fromBtcTx tx@BtcTx{..}
        | btcVer == 1 && btcLock == Nothing = SigSinglePair input output
        | otherwise = error $ "Modified transaction data: " ++ show tx
            where input  = head . NE.toList $ btcIns
                  output = head btcOuts

mkSigSinglePair :: InputG r () -> BtcOut -> SigSinglePair r ()
mkSigSinglePair pin out =
    SigSinglePair (adjustSignFlag pin) out
  where
    adjustSignFlag = setSignFlag $
        if btcAmount out /= nullAmount
            then HS.SigSingle True
            else HS.SigNone   True

signPair :: ( Monad m, Show t
            , TransformSigData BtcSig () r, SignatureScript t BtcSig
            , SpendFulfillment BtcSig r, HasSpendCond r t ) =>
       HC.PrvKeyC
    -> SigSinglePair t ()
    -> m (Either BtcError (SigSinglePair t BtcSig))
signPair prvKey sp@SigSinglePair{..} =
    fmap fromBtcTx <$> signTx (const $ return prvKey) (toBtcTx sp)

singlePairVerifySig ::
   ( Show ss, Show t
   , SpendFulfillment ss r
   , SpendCondition r
   , HasSpendCond r t
   ) =>
   SigSinglePair t ss -> Either VerifyError ()
singlePairVerifySig sp = verifyTx (toBtcTx sp)

toClientSignedTx :: Show t => NE.NonEmpty (SigSinglePair t BtcSig) -> BtcTx t BtcSig
toClientSignedTx spL = foldr addInOut (toBtcTx $ NE.last spL) (NE.init spL)
    where addInOut SigSinglePair{..} tx@BtcTx{..} =
             tx { btcIns  = singleInput NE.<| btcIns
                , btcOuts = singleOutput : btcOuts
                }


getSigData :: SigSinglePair t sd -> sd
getSigData = btcSigData . singleInput

pairRedeemScript :: HasSpendCond r t => SigSinglePair t a -> r
pairRedeemScript SigSinglePair{..} = inputCondScript singleInput

fundingValue :: SigSinglePair t a -> BtcAmount
fundingValue SigSinglePair{..} = btcInValue singleInput

clientChangeVal :: SigSinglePair t BtcSig -> BtcAmount
clientChangeVal SigSinglePair{..} = nonDusty (btcAmount singleOutput)

resetClientChangeVal :: SigSinglePair t a -> Either BtcError (SigSinglePair t a)
resetClientChangeVal ssp@SigSinglePair{..} =
    mkNew <$> mkNonDusty (fundingValue ssp)
  where
    mkNew fundVal =
        ssp { singleOutput =
                 singleOutput { btcAmount = fundVal }
            }


clientChangeAddr :: SigSinglePair t BtcSig -> HC.Address
clientChangeAddr SigSinglePair{..} = btcAddress singleOutput

clearSig :: SigSinglePair t BtcSig -> SigSinglePair t ()
clearSig = spMapSigData $ const ()

spMapSigData :: (a -> b) -> SigSinglePair t a -> SigSinglePair t b
spMapSigData f sp@SigSinglePair{..} = sp { singleInput = mapSigData f singleInput }


-- | After the lockTime -- specified in the funding output in the blockchain --
--    has passed, the client/sender can redeem all sent funds. So we make sure
--    this isn't possible before accepting a payment.
fundingIsLocked :: (Show t, Show sd, MonadTime m, HasLockTimeDate t) => SigSinglePair t sd -> m Bool
fundingIsLocked = allInputsLocked . toBtcTx

