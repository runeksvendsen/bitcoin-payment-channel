{-# LANGUAGE ScopedTypeVariables #-}
module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
(
  module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
, module Data.Bitcoin.PaymentChannel.Internal.Util
)
where


import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount

import qualified Data.Serialize as Bin
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Util as HU
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC
import           Data.Word (Word32)
import Data.String (fromString)


calcTxSize :: HT.Tx -> Word
calcTxSize = fromIntegral . B.length . Bin.encode

dummyHash256 = fromString "3d96c573baf8f782e5f5f33dc8ce3c5bae654cbc888e9a3bbb8185a75febfd76" :: HC.Hash256


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

mkTxOut :: (HC.Address, BitcoinAmount) -> HT.TxOut
mkTxOut (adr,val) = HT.TxOut
    (fromIntegral . toInteger $ val)
    (addressToScriptPubKeyBS adr)
