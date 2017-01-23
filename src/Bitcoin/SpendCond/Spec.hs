module Bitcoin.SpendCond.Spec
(
  spendCondSpec
)
where

import Bitcoin.SpendCond.Cond
import Bitcoin.Util

import Test.Hspec
import Network.Haskoin.Script
import qualified Network.Haskoin.Script as HS



data TestCond    = TestCond     -- ^ Test condition script
data TestFulfill = TestFulfill  -- ^ Test fulfillment/signature script

instance SpendCondition TestCond where
    conditionScript _   = Script [ opPushData "TEST_SCRIPT" ]
instance SpendFulfillment TestFulfill TestCond where
    signatureScript _ _ = Script [ opPushData "TEST_SIG" ]
    rawSigs = undefined

testPubKey = mkDummyPubkey "0323dcd0a7481cb90e3583923701b14d0b6757ebd76bf5b49e696c61f193d7a489"
testSig    = mkDummySig    "304402204202cdb61cb702aa62de312a8e5eada817d90c4e26c8b696780b14d1576f204f02203c134d0acb057d917508ca9baab241a4f66ebea32f7acceeaf621a334927e17701"
testBtcSig = MkBtcSig testSig (HS.SigAll False)

fulfill    = TestFulfill
testPKHSig = SpendPKH testBtcSig

-- | For regular P2PKH we're using an actual condition/script,
--    but for P2WPKH it's just a template that's
--    handled as a special case by the script interpreter.
p2pkh        = Pay2 $                 Cond $ PubkeyHash testPubKey
p2wpkh       = Pay2 $              Witness $ PubkeyHash testPubKey
p2wpkhINp2sh = Pay2 $ ScriptHash $ Witness $ PubkeyHash testPubKey

p2sh         = Pay2 $           ScriptHash $ Cond TestCond
p2wsh        = Pay2 $              Witness $ Cond TestCond
p2wshINp2sh  = Pay2 $ ScriptHash $ Witness $ Cond TestCond



spendCondSpec :: IO ()
spendCondSpec = hspec $
  describe "Transaction script & witness" $ do
    describe "P2PKH has correct" $ do
        it "scriptPubKey" $ scriptPubKey p2pkh `shouldBe`
            TxOutputScript [ OP_DUP
                           , OP_HASH160
                           , opPush (hash160 testPubKey)
                           , OP_EQUALVERIFY
                           , OP_CHECKSIG
                           ]
        it "scriptSig" $ inputScript testPKHSig p2pkh `shouldBe`
            TxInputScript  [ opPush testBtcSig, opPush testPubKey ]
        it "witness" $ witnessScript testPKHSig p2pkh `shouldBe`
            WitnessScript mempty


    describe "P2SH has correct" $ do
        it "scriptPubKey" $ scriptPubKey p2sh `shouldBe`
            TxOutputScript [ OP_HASH160
                           , opPush $ hash160 (conditionScript TestCond)
                           , OP_EQUAL
                           ]
        it "scriptSig" $ inputScript fulfill p2sh `shouldBe`
            TxInputScript  [ OP_PUSHDATA "TEST_SIG" OPCODE
                           , OP_PUSHDATA "\vTEST_SCRIPT" OPCODE
                           ]
        it "witness" $ witnessScript fulfill p2sh `shouldBe`
            WitnessScript mempty

    describe "P2WPKH has correct" $ do
        it "scriptPubKey" $ scriptPubKey p2wpkh `shouldBe`
            TxOutputScript [ OP_0
                           , opPush (hash160 testPubKey)
                           ]
        it "scriptSig" $ inputScript testPKHSig p2wpkh `shouldBe`
            TxInputScript  mempty
        it "witness" $ witnessScript testPKHSig p2wpkh `shouldBe`
            WitnessScript [ opPush testBtcSig, opPush testPubKey ]

    describe "P2WPKH in P2SH has correct" $ do
        it "scriptPubKey" $ scriptPubKey p2wpkhINp2sh `shouldBe`
            TxOutputScript [ OP_HASH160
                           , OP_PUSHDATA "sA\SOH\aR\166\&1\196\182\178%\US\183[\177g^\158E\177" OPCODE
                           , OP_EQUAL
                           ]
        it "scriptSig" $ inputScript testPKHSig p2wpkhINp2sh `shouldBe`
            TxInputScript  [ OP_PUSHDATA (serialize $ Script [ OP_0, opPush (hash160 testPubKey) ]) OPCODE ]
        it "witness" $ witnessScript testPKHSig p2wpkhINp2sh `shouldBe`
            WitnessScript [ opPush testBtcSig, opPush testPubKey ]

    describe "P2WSH has correct" $ do
        it "scriptPubKey" $ scriptPubKey p2wsh `shouldBe`
            TxOutputScript [ OP_0
                           , opPush (hash256 $ conditionScript TestCond)
                           ]
        it "scriptSig" $ inputScript fulfill p2wsh `shouldBe`
            TxInputScript  mempty
        it "witness" $ witnessScript fulfill p2wsh `shouldBe`
            WitnessScript [ OP_PUSHDATA "TEST_SIG" OPCODE
                          , OP_PUSHDATA "\vTEST_SCRIPT" OPCODE
                          ]

    describe "P2WSH in P2SH has correct" $ do
        it "scriptPubKey" $ scriptPubKey p2wshINp2sh `shouldBe`
            TxOutputScript [ OP_HASH160
                           , OP_PUSHDATA "tq\233\219J\199\255\192C\177n\"\156U\171\233\151\206\188O" OPCODE
                           , OP_EQUAL
                           ]
        it "scriptSig" $ inputScript fulfill p2wshINp2sh `shouldBe`
            TxInputScript  [ OP_PUSHDATA (serialize $ Script [ OP_0, opPush (hash256 $ conditionScript TestCond) ]) OPCODE ]
        it "witness" $ witnessScript fulfill p2wshINp2sh `shouldBe`
            WitnessScript [ OP_PUSHDATA "TEST_SIG" OPCODE
                          , OP_PUSHDATA "\vTEST_SCRIPT" OPCODE
                          ]

