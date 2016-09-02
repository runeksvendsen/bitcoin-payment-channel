{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util where


import qualified Data.Binary as Bin
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC


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

replaceScriptInput :: B.ByteString -> HT.Tx -> HT.Tx
replaceScriptInput scriptIn (HT.Tx v (txIn:_) txOut lt)  =
    HT.Tx v [newTxIn] txOut lt
        where newTxIn = txIn { HT.scriptInput = scriptIn }
replaceScriptInput _ (HT.Tx _ [] _ _) =
    error "cannot replace scriptInput without any inputs"

removeOutputs :: HT.Tx -> HT.Tx
removeOutputs tx = tx { HT.txOut = [] }

appendOutput :: HT.Tx -> HT.TxOut -> HT.Tx
appendOutput tx@HT.Tx{ HT.txOut = oldOuts } txOut =
    tx { HT.txOut = oldOuts ++ [txOut] }

bitcoinPayPK :: HC.PubKey -> HS.Script
bitcoinPayPK pk = HS.encodeOutput $ HS.PayPKHash $ HC.pubKeyAddr pk
bitcoinPayPKBS = serialize . bitcoinPayPK
    where serialize = BL.toStrict . Bin.encode