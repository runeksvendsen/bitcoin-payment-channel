{-# LANGUAGE DeriveGeneric, DeriveAnyClass, KindSignatures #-}
module Bitcoin.SpendCond.Cond
( module Bitcoin.SpendCond.Cond
, module Bitcoin.Types
)
where

import Bitcoin.Util
import Bitcoin.Internal.Util
import Bitcoin.Types
import Network.Haskoin.Script
import Data.Void

import qualified Data.Serialize         as Bin
import qualified Data.Aeson.Types       as JSON
import qualified Data.ByteString        as B


-- ### Input interface
-- #####################
-- | Script that defines a condition to spend
class Show c => SpendCondition c where
    -- | The script will, depending on transaction type,
    --    be placed either in output, input or witness of transaction
    conditionScript :: c -> Script

-- | Script that can spend funds sent to 'conditionScript'
class SpendFulfillment f c where
    -- | Script fulfilling 'conditionScript'
    signatureScript :: f -> c -> Script
    -- | Return all pubkeys-and-signature pairs from "f" and "c". Used for signature verification.
    rawSigs         :: f -> c -> [(PubKey, BtcSig)]


-- ### Output interface
-- ######################
-- | Script we put in the output of a transaction
class ScriptPubKey c t where
    scriptPubKey :: c -> TxOutputScript t

class SignatureScript c f t where
    -- | The script we put inside a transaction input
    inputScript     :: f -> c -> TxInputScript t
    -- | The script we put inside a transaction witness
    witnessScript   :: f -> c -> WitnessScript t


-- | Pay to something
newtype Pay2 a = Pay2 a
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize, NFData)

-- | Turns something into its SegWit counterpart
--newtype Witness a = Witness a
--    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize, NFData)

-- | Hash a 'SpendCondition'
newtype ScriptHash a = ScriptHash a
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize, NFData)

-- | Represents a 'SpendCondition'
data Cond = Cond
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize, NFData)

type P2S = Pay2 Cond
type P2SH = Pay2 (ScriptHash Cond)

---- | Pay to script (P2S)
--newtype P2Script = P2Script (Pay2 Cond)
---- | Pay to script hash (P2SH)
--newtype P2SH     = P2SH (Pay2 (ScriptHash Cond))
---- | Pay to witness script hash (P2WSH)
--newtype P2WSH    = P2WSH (Pay2 (Witness Cond))
---- | P2WSH inside P2SH (for compatibility with old wallet clients)
--newtype P2SHWit  = P2SHWit (Pay2 (ScriptHash (Witness Cond)))

--mkP2sh :: c -> P2SH c
--mkP2sh = Pay2 . ScriptHash . Cond

--class IsWrapper c (wrap :: * -> *) where
--    unwrap :: wrap c -> c
--
--instance IsWrapper c Pay2       where unwrap (Pay2 c) = c
--instance IsWrapper c Cond       where unwrap Cond = c
--instance IsWrapper c Witness    where unwrap (Witness c) = c
--instance IsWrapper c ScriptHash where unwrap (ScriptHash c) = c

--class HasSpendCond c (ac :: * -> *) where
--    getCond :: ac c -> c
--    mkCond  :: c    -> ac c
--
--instance HasSpendCond c P2Script where
--    getCond = unwrap . unwrap
--    mkCond  = P2Script . Pay2 . Cond
--instance HasSpendCond c P2SH where
--    getCond = unwrap . unwrap . unwrap
--    mkCond  = P2SH . Pay2 . ScriptHash . Cond
--instance HasSpendCond c P2WSH where
--    getCond = unwrap . unwrap . unwrap
--    mkCond  = P2WSH . Pay2 . Witness . Cond
--instance HasSpendCond c P2SHWit where
--    getCond = unwrap . unwrap . unwrap . unwrap
--    mkCond  = P2SHWit . Pay2 . ScriptHash . Witness . Cond
--
--inputCondScript :: HasSpendCond r t => InputG t r a -> r
--inputCondScript MkInputG{..} = getCond btcInType


-- ## ScriptPubKey
-- #################
instance SpendCondition c => ScriptPubKey c P2S where
    scriptPubKey c = mkScript $
        conditionScript c

instance SpendCondition c => ScriptPubKey c P2SH where
    scriptPubKey c =
        p2shScriptPubKey (mkScript $ conditionScript c)

{-
instance -- WitnessProgram wp
      SpendCondition c => ScriptPubKey c (Pay2 (Witness Cond)) where
    scriptPubKey wp = TxOutputScript
        [ OP_0, opPushData $ witnessProgram wp ]

instance -- WitnessProgram wp
      SpendCondition c => ScriptPubKey c (Pay2 (ScriptHash (Witness Cond))) where
    scriptPubKey wp =
        p2shScriptPubKey (scriptPubKey wp :: TxOutputScript (Pay2 (Witness Cond)))

-}
-- ## SignatureScript
-- ####################

-- Pay to script
instance SpendFulfillment f c => SignatureScript c f P2S where
    inputScript ss c = mkScript $
        signatureScript ss c
    witnessScript _ _ = WitnessScript []

-- P2SH
instance ( SpendCondition c, SpendFulfillment f c)
        => SignatureScript c f P2SH where
    -- For P2SH inputScript, we add a push of the serialized conditionScript
    inputScript ss c = mkScript $
        signatureScript ss c <> Script [ opPush $ conditionScript c ]
    witnessScript _ _ = WitnessScript []

{-
-- P2WSH/P2WPKH
instance ( SpendCondition c
         , SpendFulfillment f c
         )
        => SignatureScript c f (Pay2 (Witness Cond)) where
    inputScript _ _ = TxInputScript []
    witnessScript ss wp = mkWitnessScript wp ss

-- P2WSH/P2WPKH inside P2SH
instance ( SpendCondition c
         , SpendFulfillment f c
--         , WitnessSig f wp (Pay2 (ScriptHash (Witness Cond)))
--         , WitnessProgram wp (Pay2 (ScriptHash (Witness Cond)))
         )
        => SignatureScript c f (Pay2 (ScriptHash (Witness Cond))) where
    inputScript   _  wp = TxInputScript
        [ opPush $ asScript $ (scriptPubKey wp :: TxInputScript (Pay2 (Witness wp))) ]
    witnessScript ss (Pay2 (ScriptHash (Witness wp))) = mkWitnessScript wp ss
-}
-- Util
p2shScriptPubKey :: TxOutputScript a -> TxOutputScript b
p2shScriptPubKey s = TxOutputScript
    [ OP_HASH160, opPush $ hash160 (asScript s), OP_EQUAL ]



-- ## P2PKH
-- ##########
data PubkeyHash = PubkeyHash PubKey
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize, NFData)
data SpendPKH   = SpendPKH   BtcSig
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize, NFData)

instance SpendCondition PubkeyHash where
    conditionScript (PubkeyHash pk) = Script
        [ OP_DUP, OP_HASH160, opPush $ hash160 pk, OP_EQUALVERIFY, OP_CHECKSIG ]

instance SpendFulfillment SpendPKH PubkeyHash where
    signatureScript (SpendPKH sig) (PubkeyHash pk) = Script [ opPush sig, opPush pk ]
    rawSigs (SpendPKH sig) (PubkeyHash pk) = [(pk, sig)]

instance SignatureScript PubkeyHash SpendPKH Void where
    inputScript s = mkScript . signatureScript s
    witnessScript _ _ = WitnessScript []


{-

-- | A witness program is (currently) either a SHA256 hash of a script
--    or RIPEMD160 hash of a compressed public key.
class WitnessProgram wp t where
    witnessProgram  :: wp -> Tagged t B.ByteString
    redeemScript    :: wp -> Tagged t Script

-- P2WSH witness program
instance SpendCondition c => WitnessProgram c Cond where
    witnessProgram c = Tagged $ Bin.encode $ hash256 (conditionScript c)
    redeemScript wp =
            Tagged $ Script [ opPush  $ Script [ OP_0, opPushData $ unTagged (witnessProgram wp) ] ]

-- P2WPKH witness program
instance WitnessProgram PubkeyHash Void where
    witnessProgram (PubkeyHash pk) = Tagged $ Bin.encode $ hash160 pk
    redeemScript _ = Tagged mempty


class WitnessSig ws wp t where
    mkWitnessScript :: wp -> (ws -> WitnessScript t)

-- P2SH witness program
instance (SpendCondition c, SpendFulfillment f c) => WitnessSig f c Cond where
    -- Same as P2SH inputScript
    mkWitnessScript c f = mkScript . asScript $
        (inputScript f c :: TxInputScript (Pay2 (ScriptHash Cond)))

-- P2WPKH witness program
instance WitnessSig SpendPKH PubkeyHash Void where
    -- Same as P2PKH inputScript
    mkWitnessScript pkh sig = mkScript . asScript $
        (inputScript sig pkh :: TxInputScript (Pay2 Cond))

-}