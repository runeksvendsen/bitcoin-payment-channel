{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bitcoin.Util where

import           Bitcoin.Internal.Util
import           Data.String                 (fromString)
import           Data.Word                   (Word32)

import qualified Data.ByteString             as B
import qualified Data.ByteString.Base16      as B16
import qualified Data.List.NonEmpty          as NE
import qualified Data.Serialize              as Bin
import qualified Network.Haskoin.Crypto      as HC
import qualified Network.Haskoin.Node        as HN
import qualified Network.Haskoin.Script      as HS
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Util        as HU



instance Monoid HS.Script where
    mempty = HS.Script []
    mappend (HS.Script scr1) (HS.Script scr2) =
        HS.Script $ scr1 `mappend` scr2


-- | Serialize NonEmpty list of "script witnesses".
--   A transaction with no witnesses should use the old tx serialization format, hence the non-empty list.
--    (https://github.com/bitcoin/bips/blob/master/bip-0144.mediawiki#Serialization)
serTxWitness :: NE.NonEmpty HS.Script -> B.ByteString
serTxWitness = NE.head . NE.scanr foldScript B.empty
    where
        foldScript scr !bsAccum = serScript scr <> bsAccum
        serScript scr = Bin.encode (HN.VarInt $ scrLen scr) <> Bin.encode scr
        scrLen (HS.Script ops) = fromIntegral $ length ops

calcTxSize :: HT.Tx -> Word
calcTxSize = fromIntegral . B.length . Bin.encode

dummyHash256 :: HC.Hash256
dummyHash256 = fromString "0000000000000000000000000000000000000000000000000000000000000000"

hash256 :: Bin.Serialize a => a -> HC.Hash256
hash256 = HC.hash256 . serialize

hash160 :: Bin.Serialize a => a -> HC.Hash160
hash160 = HC.hash160 . HC.getHash256 . HC.hash256 . serialize

opPush :: Bin.Serialize a => a -> HS.ScriptOp
opPush = HS.opPushData . Bin.encode

addressToScript :: HC.Address -> HS.ScriptOutput
addressToScript addr =
    case addr of
        a@(HC.PubKeyAddress _) -> HS.PayPKHash a
        a@(HC.ScriptAddress _) -> HS.PayScriptHash a

decodeScriptHash :: B.ByteString -> Either String HC.Hash160
decodeScriptHash bs =
    HS.decodeOutputBS bs >>=
    \scrOut -> case scrOut of
        HS.PayScriptHash a -> getP2SHAddr a
        x                  -> Left $ "Expected P2SH output, found: " ++ show x
  where
    getP2SHAddr (HC.ScriptAddress h) = Right h
    getP2SHAddr (HC.PubKeyAddress _) = Left "Expected P2SH address, found P2PKH address"

addressToScriptPubKeyBS :: HC.Address -> B.ByteString
addressToScriptPubKeyBS = HS.encodeOutputBS . addressToScript

replaceScriptInput :: Word32 -> HS.Script -> HT.Tx -> HT.Tx
replaceScriptInput index scriptIn tx =
    HT.createTx (HT.txVersion tx) newTxIns (HT.txOut tx) (HT.txLockTime tx)
        where newTxIns = HU.updateIndex (fromIntegral index) (HT.txIn tx) replaceScriptIn
              replaceScriptIn txIn = txIn { HT.scriptInput = Bin.encode scriptIn}

removeOutputs :: HT.Tx -> HT.Tx
removeOutputs tx =
    HT.createTx (HT.txVersion tx) (HT.txIn tx) [] (HT.txLockTime tx)

appendOutput :: HT.Tx -> HT.TxOut -> HT.Tx
appendOutput tx txOut =
    HT.createTx (HT.txVersion tx) (HT.txIn tx) ( oldOuts ++ [txOut] ) (HT.txLockTime tx)
        where oldOuts = HT.txOut tx

bitcoinPayPK :: HC.PubKeyC -> HS.Script
bitcoinPayPK pk = HS.encodeOutput $ HS.PayPKHash $ HC.pubKeyAddr pk
bitcoinPayPKBS = serialize . bitcoinPayPK
    where serialize = Bin.encode


