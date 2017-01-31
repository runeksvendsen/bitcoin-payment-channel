{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Bitcoin.Compare where

import Bitcoin.SpendCond.Cond
import Bitcoin.Util

import           Data.Word                      (Word32)
import qualified Data.List.NonEmpty             as NE
import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import GHC.Generics       (Generic)


data DiffInfo = DiffInfo [(HC.Address,ValDiff)]

data ValDiff =
    Increase BtcAmount
  | Decrease BtcAmount
  | NoChange
        deriving (Eq, Show, Generic, NFData)

data TxMismatch r =
    TxVersionMismatch Word32 Word32
  | TxLocktimeMismatch (Maybe LockTimeDate) (Maybe LockTimeDate)
  | TxInMismatch (InMismatch r)
  | TxOutMisMatch OutMismatch
        deriving (Eq, Show, Generic, NFData)

data InMismatch r =
    InPrevOutMismatch HT.OutPoint HT.OutPoint
  | InRdmScrMismatch r r
  | InSequenceMismatch Word32 Word32
        deriving (Eq, Show, Generic, NFData)

data OutMismatch =
    OutAddressMismatch HC.Address HC.Address
        deriving (Eq, Show, Generic, NFData)


-- | Compare two transactions, ignoring output amounts and signature data, and return either
--    a) mismatch error
--    b) the difference in value for each output, between the two transactions.
--   Eg. "valueDiff tx1 tx2", where tx1 pays 100000 satoshi to a single address "adr" and tx2
--     is the same except the value paid is 90000 satoshi, will return "DiffInfo [adr, Decrease 10000]
valueDiff :: forall r t a. (HasSpendCond r t, Eq t, Eq r) =>
    BtcTx t a -> BtcTx t a -> Either (TxMismatch r) DiffInfo
valueDiff oldTx newTx =
       compareProp oldTx newTx btcVer TxVersionMismatch
    >> compareProp oldTx newTx btcLock TxLocktimeMismatch
    >>              getFirstErr TxInMismatch compareIns
    >> DiffInfo <$> getFirstErr TxOutMisMatch compareOuts

  where
    getFirstErr mkErr eL = if not $ null (lefts eL) then Left $ mkErr (head $ lefts eL) else Right (rights eL)
    compareIns = zipWith inputDiff (NE.toList $ btcIns oldTx) (NE.toList $ btcIns newTx)
    compareOuts = zipWith outputDiff (btcOuts oldTx) (btcOuts newTx)


inputDiff :: (HasSpendCond r t, Eq t, Eq r) => InputG t a -> InputG t a -> Either (InMismatch r) ()
inputDiff oldIn newIn =
       compareProp oldIn newIn btcPrevOut   InPrevOutMismatch
    >> compareProp oldIn newIn inputCondScript InRdmScrMismatch
    >> compareProp oldIn newIn btcSequence  InSequenceMismatch
    >> return ()

outputDiff :: BtcOut -> BtcOut -> Either OutMismatch (HC.Address,ValDiff)
outputDiff oldOut newOut =
    compareProp oldOut newOut btcAddress OutAddressMismatch
    >>= \addr -> Right (addr,valDiff)
  where
    newAmount = nonDusty (btcAmount newOut)
    oldAmount = nonDusty (btcAmount oldOut)
    valDiff
        | newAmount == oldAmount = NoChange
        | newAmount > oldAmount  = Increase (newAmount - oldAmount)
        | otherwise              = Decrease (oldAmount - newAmount)

-- Check everything is equal except: 1) signature data (sig hash flag IS checked)
--  2) output amounts
eqIgnoreOutVal :: Eq t =>
    IgnoreSigData (BtcTx t BtcSig) -> IgnoreSigData (BtcTx t BtcSig) -> Bool
eqIgnoreOutVal tx1 tx2 =
    fmap txClearOutVal tx1 == fmap txClearOutVal tx2
  where
    txClearOutVal  tx  = tx { btcOuts = map outputValClear (btcOuts tx) }
    outputValClear out = out { btcAmount = nullAmount }


-- Util
compareProp :: Eq prop => a -> a -> (a -> prop) -> (prop -> prop -> e) -> Either e prop
compareProp old new getProp mkErr =
    if getProp old == getProp new
        then Right (getProp new)
        else Left $ mkErr (getProp old) (getProp new)

