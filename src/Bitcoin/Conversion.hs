module Bitcoin.Conversion
(
  module Bitcoin.Conversion
, module Bitcoin.Types
, module Bitcoin.SpendCond.Cond
)
where

import Bitcoin.SpendCond.Cond
import Bitcoin.Types

import qualified Data.List.NonEmpty         as NE
import qualified Data.ByteString            as B
import qualified Data.Serialize             as Bin
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto     as HC


toTxOut :: ScriptPubKey outType => OutputG outType -> HT.TxOut
toTxOut MkOutputG{..} = HT.TxOut
    (fromIntegral . toInteger $ btcOutAmount)
    (Bin.encode $ asScript $ scriptPubKey btcOutType)

toInRaw :: B.ByteString -> InputG inType sigData -> HT.TxIn
toInRaw inpScr MkInputG{..} = HT.TxIn btcPrevOut inpScr btcSequence

toUnsigTxIn :: InputG inType sigData -> HT.TxIn
toUnsigTxIn = toInRaw B.empty

toSigTxIn :: SignatureScript t sigData => InputG t sigData -> HT.TxIn
toSigTxIn btcIn@MkInputG{..} =
    toInRaw (Bin.encode $ asScript $ inputScript btcSigData btcInType) btcIn


toTxOut' :: BtcOut -> HT.TxOut
toTxOut' (BtcOut adr val) =
    toTxOutRaw (adr, nonDusty val)

toTxOutRaw :: (HC.Address, BtcAmount) -> HT.TxOut
toTxOutRaw (adr, val) = HT.TxOut
    (fromIntegral . toInteger $ val)
    (addressToScriptPubKeyBS adr)

toChangeTxOut :: BtcAmount -> ChangeOut -> HT.TxOut
toChangeTxOut val ChangeOut{..} =
    toTxOutRaw (btcChangeAddr, val)

getAllOuts :: BtcTx r a -> [HT.TxOut]
getAllOuts BtcTx{..} =
    map toTxOut' finalOuts ++ maybe [] mkChgOut btcChgOut
    where
        -- | Remove zero-value outputs
        finalOuts = filter ((/= nullAmount) . btcAmount) btcOuts
        inVal = sum . NE.toList $ NE.map btcInValue btcIns
        outVal = sum $ map (nonDusty . btcAmount) btcOuts
        mkChgOut co = [ toChangeTxOut (inVal - outVal - btcAbsFee_ co) co ]


toUnsignedTx :: BtcTx r a -> HT.Tx
toUnsignedTx tx@BtcTx{..} = toTxWithIns
    (NE.map toUnsigTxIn btcIns) tx

toTxWithIns :: NE.NonEmpty HT.TxIn -> BtcTx r a -> HT.Tx
toTxWithIns binL tx@BtcTx{..} = HT.createTx
    btcVer
    (NE.toList binL)
    (getAllOuts tx)
    (maybe 0 toWord32 btcLock)

toHaskoinTx :: SignatureScript t ss => BtcTx t ss -> HT.Tx
toHaskoinTx tx@BtcTx{..} = toTxWithIns
    (NE.map toSigTxIn btcIns) tx

