module Bitcoin.Conversion
(
  module Bitcoin.Conversion
, module Bitcoin.Types
, module Bitcoin.SpendCond.Cond
)
where

import Bitcoin.SpendCond.Cond
import Bitcoin.Types
import Bitcoin.Util
import Bitcoin.Internal.Types

import qualified Data.List.NonEmpty         as NE
import qualified Data.ByteString            as B
import qualified Data.Serialize             as Bin
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto     as HC


toTxOut :: forall r outType. ScriptPubKey r outType => OutputG outType r -> HT.TxOut
toTxOut MkOutputG{..} = HT.TxOut
    (fromIntegral . toInteger $ btcOutAmount)
    (Bin.encode $ asScript (scriptPubKey btcOutCond :: TxOutputScript outType))

toInRaw :: B.ByteString -> InputG inType r sigData -> HT.TxIn
toInRaw inpScr MkInputG{..} = HT.TxIn btcPrevOut inpScr btcSequence

toUnsigTxIn :: InputG inType r sigData -> HT.TxIn
toUnsigTxIn = toInRaw B.empty

toSigTxIn :: forall r sigData t. SignatureScript r sigData t => InputG t r sigData -> HT.TxIn
toSigTxIn btcIn@MkInputG{..} =
    toInRaw (Bin.encode $ asScript (inputScript btcSigData btcCondScr :: TxInputScript t)) btcIn

-- | Derive dummy private key from an input's prev_out txid
dummyPrvKey :: InputG t r sd -> HC.PrvKeyC
dummyPrvKey MkInputG{..} =
    fromMaybe (error "Failed to decode PrvKeyC from 32 bytes")
        . HC.decodePrvKey HC.makePrvKeyC . Bin.encode . HT.outPointHash $ btcPrevOut


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

getAllOuts :: BtcTx t r sd -> [HT.TxOut]
getAllOuts BtcTx{..} =
    map toTxOut' finalOuts ++ maybe [] mkChgOut btcChgOut
    where
        btcAbsFee_ = getAbsFee . btcTxFee
        getAbsFee (AbsoluteFee val) = val
        getAbsFee (RelativeFee _) = error "BUG. getAllOuts: relative fee"
        -- | Remove zero-value outputs
        finalOuts = filter ((/= nullAmount) . btcAmount) btcOuts
        inVal = sum . NE.toList $ NE.map btcInValue btcIns
        outVal = sum $ map (nonDusty . btcAmount) btcOuts
        mkChgOut co = [ toChangeTxOut (inVal - outVal - btcAbsFee_ co) co ]


toUnsignedTx :: BtcTx t r sd -> HT.Tx
toUnsignedTx tx@BtcTx{..} = toTxWithIns
    (NE.map toUnsigTxIn btcIns) tx

toTxWithIns :: NE.NonEmpty HT.TxIn -> BtcTx t r sd -> HT.Tx
toTxWithIns binL tx@BtcTx{..} = HT.createTx
    btcVer
    (NE.toList binL)
    (getAllOuts tx)
    (maybe 0 toWord32 btcLock)

toHaskoinTx :: SignatureScript r ss t => BtcTx t r ss -> HT.Tx
toHaskoinTx tx@BtcTx{..} = toTxWithIns
    (NE.map toSigTxIn btcIns) tx

