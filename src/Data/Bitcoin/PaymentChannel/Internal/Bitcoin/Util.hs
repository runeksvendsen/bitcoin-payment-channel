{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util where


import qualified Data.Serialize as Ser
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Util as HU
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC
import           Data.Word (Word32)


-- | Converts a pay-to-pubkey-hash address string to Script.
-- | Eg. \"1PDGytNA7EJ5XrJdTGKv11VAUFxKnsfwke\" into
-- | \"Script [OP_DUP, OP_HASH160, OP_PUSHDATA f3a5194fbf4b3556838e0f773b613a876737a2c6,
-- | OP_EQUALVERIFY, OP_CHECKSIG]\"
p2PKAddressToScript :: String -> Maybe HS.Script
p2PKAddressToScript addrStr =
    HS.encodeOutput . HS.PayPKHash <$> HC.base58ToAddr (C.pack addrStr)

-- | Converts a pay-to-script-hash address string to Script.
-- | Eg. \"2PDGytNA7EJ5XrJdTGKv11VAUFxKnsfwke\"
p2SHAddressToScript :: String -> Maybe HS.Script
p2SHAddressToScript addrStr =
    HS.encodeOutput . HS.PayScriptHash <$> HC.base58ToAddr (C.pack addrStr)

addressToScript :: HC.Address -> HS.ScriptOutput
addressToScript addr =
    case addr of
        a@(HC.PubKeyAddress _) -> HS.PayPKHash a
        a@(HC.ScriptAddress _) -> HS.PayScriptHash a

addressToScriptPubKeyBS :: HC.Address -> B.ByteString
addressToScriptPubKeyBS = HS.encodeOutputBS . addressToScript

replaceScriptInput :: Word32 -> B.ByteString -> HT.Tx -> HT.Tx
replaceScriptInput index scriptIn tx =
    HT.createTx (HT.txVersion tx) newTxIns (HT.txOut tx) (HT.txLockTime tx)
        where newTxIns = HU.updateIndex (fromIntegral index) (HT.txIn tx) replaceScriptIn
              replaceScriptIn txIn = txIn { HT.scriptInput = scriptIn}

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
    where serialize = Ser.encode
