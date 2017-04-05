module Bitcoin.SpendCond.Util
( getPrevIns
, singlePrevIn
)
where

import Bitcoin.Util
import qualified Network.Haskoin.Transaction as HT
import Bitcoin.SpendCond.Cond
import Data.Word (Word32)
import Debug.Trace


-- | Create inputs that redeem outputs paying to the given (P2SH) redeemScript
getPrevIns :: SpendCondition r =>
       HT.Tx
    -> r
    -> [InputG (P2SH r) ()]
getPrevIns tx rdmScr =
    map (mapInputType mkCond . mkInput) $ catMaybes $
        checkOuts libCheckOut
  where
    mkInput (idx,val) = mkNoSigTxIn (mkPrevOut idx) (fromIntegral val) rdmScr
    mkPrevOut = HT.OutPoint (HT.txHash tx)
    checkOuts f = zipWith f [0..] (HT.txOut tx)
    -- implementation: Bitcoin.SpendCond
    libCheckOut idx out =
        if HT.scriptOutput out == encode (asScript $ scriptPubKey (mkP2sh rdmScr))
            then Just (idx, HT.outValue out)
            else Nothing
    -- implementation: Network.Haskoin
    haskoinCheckOut idx out =
            either (const Nothing) (_checkMatch idx)
                (decodeScriptHash (HT.scriptOutput out) >>= \sh -> Right (sh, HT.outValue out))
    _checkMatch idx (hash,val) = if hash == _scrHash then Just (idx,val) else Nothing
    _scrHash = hash160 $ conditionScript rdmScr


singlePrevIn :: (Show r, SpendCondition r) => HT.Tx -> r -> Word32 -> Maybe (InputG (P2SH r) ())
singlePrevIn tx scr i =
    let inputOfInterest = (== i) . HT.outPointIndex . btcPrevOut in
    case filter inputOfInterest $ getPrevIns tx scr of
      []    -> Nothing
      [inp] -> Just inp
      x     -> error $ "getPrevIns: multiple inputs with same prevOutIndex: " ++ show x
