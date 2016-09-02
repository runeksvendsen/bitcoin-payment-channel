-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Serialization
import Data.Bitcoin.PaymentChannel.Internal.Util

import qualified  Network.Haskoin.Internals as HI
import qualified  Network.Haskoin.Crypto as HC
import Network.Haskoin.Script
    (Script(..), SigHash(..), ScriptOp(..), opPushData)

import qualified Data.ByteString as B
import Data.Word

valReceiverSigHash = SigAll True

-- |Generates OP_CHECKLOCKTIMEVERIFY redeemScript
paymentChannelRedeemScript :: SendPubKey -> RecvPubKey -> Word32 -> Script
paymentChannelRedeemScript clientPK serverPK lockTime = Script
     [OP_IF,
         opPushData $ serialize (getPubKey serverPK), OP_CHECKSIGVERIFY,
     OP_ELSE,
         encodeScriptInt lockTime, op_CHECKLOCKTIMEVERIFY, OP_DROP,
     OP_ENDIF,
     opPushData $ serialize (getPubKey clientPK), OP_CHECKSIG]
         where encodeScriptInt i = opPushData $ B.pack $ HI.encodeInt (fromIntegral i)
            -- Note: HI.encodeInt encodes values up to and including 2^31-1 as 4 bytes
            --      and values 2^31 through 2^32-1 (upper limit) as 5 bytes.

-- |scriptSig fulfilling a payment redeemScript
paymentTxScriptSig :: PaymentSignature -> PaymentSignature -> Script --ScriptSig
paymentTxScriptSig clientSig serverSig = Script
    [opPushData $ serialize clientSig, --sig including SigHash byte
    opPushData $ serialize serverSig, --sig including SigHash byte
    OP_1]   -- Signal that we want to provide both PubKeys

-- |scriptSig used for the valueSender refund transaction
refundTxScriptSig :: HC.Signature -> Script
refundTxScriptSig clientSig = Script
    [opPushData (B.append (serialize clientSig) hashTypeByte),
    OP_0]   -- Signal that we want to provide only one PubKey
        where hashTypeByte = serialize (SigAll False)

-----Util-----

op_CHECKLOCKTIMEVERIFY = OP_NOP2

scriptToP2SHAddress :: Script -> HC.Address
scriptToP2SHAddress = HC.ScriptAddress . HC.hash160 . HC.getHash256 . HC.hash256 . serialize

getP2SHFundingAddress :: ChannelParameters -> HC.Address
getP2SHFundingAddress = scriptToP2SHAddress . getRedeemScript

getRedeemScript :: ChannelParameters -> Script --RedeemScript
getRedeemScript (CChannelParameters senderPK recvrPK lockTime _) =
    paymentChannelRedeemScript senderPK recvrPK (toWord32 lockTime)

getRedeemScriptBS :: ChannelParameters -> B.ByteString
getRedeemScriptBS = serialize . getRedeemScript

getInputScript :: ChannelParameters -> Script -> Script
getInputScript cp scriptSig =
    Script $ scriptOps scriptSig ++ redeemScript
        where
             redeemScript = [opPushData $ getRedeemScriptBS cp]

