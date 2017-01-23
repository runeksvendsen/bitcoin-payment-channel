{-# LANGUAGE DeriveGeneric, DeriveAnyClass, KindSignatures #-}
module Bitcoin.SpendCond.Cond
(
  module Bitcoin.SpendCond.Cond
, module Bitcoin.Types
)
where

import Bitcoin.Util
import Bitcoin.Types
import Network.Haskoin.Script

import qualified Data.Serialize         as Bin
import qualified Data.Aeson.Types       as JSON
import qualified Data.ByteString        as B


-- ### Input interface
-- #####################
-- | Script that defines a condition to spend
class SpendCondition c where
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
class ScriptPubKey c where
    scriptPubKey :: c -> TxOutputScript

class SignatureScript c f where
    -- | The script we put inside a transaction input
    inputScript     :: f -> c -> TxInputScript
    -- | The script we put inside a transaction witness
    witnessScript   :: f -> c -> WitnessScript


-- | Pay to something
data Pay2 a = Pay2 a
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize)

-- | Turns something into its SegWit counterpart
data Witness a = Witness a
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize)

-- | Hash a 'SpendCondition'
data ScriptHash a = ScriptHash a
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize)

-- | Wraps a 'SpendCondition'
data Cond a = Cond a
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize)

-- | Pay to script hash (P2SH)
type P2SH c    = Pay2 (ScriptHash (Cond c))
-- | Pay to witness script hash (P2WSH)
type P2WSH c   = Pay2 (Witness (Cond c))
-- | P2WSH inside P2SH (for compatibility with old wallet clients)
type P2SHWit c = Pay2 (ScriptHash (Witness (Cond c)))



class IsWrapper c (wrap :: * -> *) where
    unwrap :: wrap c -> c

instance IsWrapper c Pay2       where unwrap (Pay2 c) = c
instance IsWrapper c Cond       where unwrap (Cond c) = c
instance IsWrapper c Witness    where unwrap (Witness c) = c
instance IsWrapper c ScriptHash where unwrap (ScriptHash c) = c


class HasSpendCond c ac | ac -> c where
    getCond :: ac -> c

instance HasSpendCond c (Pay2 (Cond c)) where
    getCond = unwrap . unwrap
instance HasSpendCond c (Pay2 (ScriptHash (Cond c))) where
    getCond = unwrap . unwrap . unwrap
instance HasSpendCond c (Pay2 (Witness (Cond c))) where
    getCond = unwrap . unwrap . unwrap
instance HasSpendCond c (Pay2 (ScriptHash (Witness (Cond c)))) where
    getCond = unwrap . unwrap . unwrap . unwrap

inputCondScript :: HasSpendCond r t => InputG t a -> r
inputCondScript MkInputG{..} = getCond btcInType


-- ## ScriptPubKey
-- #################
instance SpendCondition c => ScriptPubKey (Pay2 (Cond c)) where
    scriptPubKey (Pay2 (Cond c)) = mkScript $
        conditionScript c

instance SpendCondition c => ScriptPubKey (Pay2 (ScriptHash (Cond c))) where
    scriptPubKey (Pay2 (ScriptHash (Cond c))) =
        p2shScriptPubKey (mkScript $ conditionScript c)

instance WitnessProgram wp => ScriptPubKey (Pay2 (Witness wp)) where
    scriptPubKey (Pay2 (Witness wp)) = TxOutputScript
        [ OP_0, opPushData $ witnessProgram wp ]

instance WitnessProgram wp => ScriptPubKey (Pay2 (ScriptHash (Witness wp))) where
    scriptPubKey (Pay2 (ScriptHash (Witness wp))) =
        p2shScriptPubKey $ scriptPubKey (Pay2 (Witness wp))


-- ## SignatureScript
-- ####################

-- Pay to script
instance SpendFulfillment f c => SignatureScript (Pay2 (Cond c)) f where
    inputScript ss (Pay2 (Cond c)) = mkScript $
        signatureScript ss c
    witnessScript _ _ = WitnessScript []

-- P2SH
instance ( SpendCondition c, SpendFulfillment f c)
        => SignatureScript (Pay2 (ScriptHash (Cond c))) f where
    -- For P2SH inputScript, we add a push of the serialized conditionScript
    inputScript ss (Pay2 (ScriptHash (Cond c))) = mkScript $
        signatureScript ss c <> Script [ opPush $ conditionScript c ]
    witnessScript _ _ = WitnessScript []

-- P2WSH/P2WPKH
instance ( WitnessSig f wp
         , WitnessProgram wp
         )
        => SignatureScript (Pay2 (Witness wp)) f where
    inputScript _ _ = TxInputScript []
    witnessScript ss (Pay2 (Witness wp)) = mkWitnessScript wp ss

-- P2WSH/P2WPKH inside P2SH
instance ( WitnessSig f wp
         , WitnessProgram wp
         )
        => SignatureScript (Pay2 (ScriptHash (Witness wp))) f where
    inputScript   _  (Pay2 (ScriptHash (Witness wp))) = TxInputScript
        [ opPush $ asScript $ scriptPubKey (Pay2 (Witness wp)) ]
    witnessScript ss (Pay2 (ScriptHash (Witness wp))) = mkWitnessScript wp ss

-- Util
p2shScriptPubKey :: TxOutputScript -> TxOutputScript
p2shScriptPubKey s = TxOutputScript
    [ OP_HASH160, opPush $ hash160 (asScript s), OP_EQUAL ]



-- ## P2PKH
-- ##########
data PubkeyHash = PubkeyHash PubKey
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize)
data SpendPKH   = SpendPKH   BtcSig
    deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize)

instance SpendCondition PubkeyHash where
    conditionScript (PubkeyHash pk) = Script
        [ OP_DUP, OP_HASH160, opPush $ hash160 pk, OP_EQUALVERIFY, OP_CHECKSIG ]

instance SpendFulfillment SpendPKH PubkeyHash where
    signatureScript (SpendPKH sig) (PubkeyHash pk) = Script [ opPush sig, opPush pk ]
    rawSigs (SpendPKH sig) (PubkeyHash pk) = [(pk, sig)]

instance SignatureScript PubkeyHash SpendPKH where
    inputScript s = mkScript . signatureScript s
    witnessScript _ _ = WitnessScript []




-- | A witness program is (currently) either a SHA256 hash of a script
--    or RIPEMD160 hash of a compressed public key.
class WitnessProgram wp where
    witnessProgram  :: wp -> B.ByteString
    redeemScript    :: wp -> Script

-- P2WSH witness program
instance SpendCondition c => WitnessProgram (Cond c) where
    witnessProgram (Cond c) = Bin.encode $ hash256 (conditionScript c)
    redeemScript wp =
            Script [ opPush  $ Script [ OP_0, opPushData $ witnessProgram wp ] ]

-- P2WPKH witness program
instance WitnessProgram PubkeyHash where
    witnessProgram (PubkeyHash pk) = Bin.encode $ hash160 pk
    redeemScript _ = mempty


class WitnessSig ws wp where
    mkWitnessScript :: wp -> (ws -> WitnessScript)

-- P2SH witness program
instance (SpendCondition c, SpendFulfillment f c) => WitnessSig f (Cond c) where
    -- Same as P2SH inputScript
    mkWitnessScript (Cond c) f = mkScript . asScript $
        inputScript f (Pay2 (ScriptHash (Cond c)))

-- P2WPKH witness program
instance WitnessSig SpendPKH PubkeyHash where
    -- Same as P2PKH inputScript
    mkWitnessScript pkh sig = mkScript . asScript $
        inputScript sig (Pay2 (Cond pkh))

