module Bitcoin.SpendCond.Util where

import Bitcoin.Util
import qualified Network.Haskoin.Transaction as HT
import Bitcoin.SpendCond.Cond


-- | Create inputs from outputs that pay to the given redeemScript
getPrevIn :: forall r t. (SpendCondition r, HasSpendCond r t) =>
       HT.Tx
    -> r
    -> [InputG t ()]
getPrevIn tx rdmScr =
    map (mapInputType mkCond . mkInput) $ catMaybes $
        zipWith checkOut [0..] (HT.txOut tx)
  where
    mkInput (idx,val) = mkNoSigTxIn (mkPrevOut idx) (fromIntegral val) rdmScr
    mkPrevOut = HT.OutPoint (HT.txHash tx)
    scrHash = hash160 $ conditionScript rdmScr
    checkMatch idx (hash,val) = if hash == scrHash then Just (idx,val) else Nothing
    checkOut idx out =
        either (const Nothing) (checkMatch idx)
            (decodeScriptHash (HT.scriptOutput out) >>= \sh -> Right (sh, HT.outValue out))
