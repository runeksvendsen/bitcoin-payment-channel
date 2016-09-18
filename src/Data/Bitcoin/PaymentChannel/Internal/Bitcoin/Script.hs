{-# LANGUAGE OverloadedStrings #-}

module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Serialization   ()
import Data.Bitcoin.PaymentChannel.Internal.Util

import qualified  Network.Haskoin.Internals as HI
import qualified  Network.Haskoin.Crypto as HC
import qualified Data.ByteString as B


valReceiverSigHash = SigAll True

-- |Generates OP_CHECKLOCKTIMEVERIFY redeemScript, which can be redeemed in two ways:
--  1) by providing a signature from both server and client
--  2) after the date specified by lockTime: by providing only a client signature
paymentChannelRedeemScript :: SendPubKey -> RecvPubKey -> BitcoinLockTime -> Script
paymentChannelRedeemScript clientPK serverPK lockTime =
    let
        -- Note: HI.encodeInt encodes values up to and including 2^31-1 as 4 bytes
        --      and values 2^31 through 2^32-1 (upper limit) as 5 bytes.
        encodeScriptInt = convertEmptyPush . opPushData . B.pack .
                          HI.encodeInt . fromIntegral . toWord32
        -- A push of an empty byte string and OP_0 are one and the same,
        --  but haskoin-core deserializes into OP_0.
        convertEmptyPush (OP_PUSHDATA "" OPCODE) = OP_0
        convertEmptyPush whatever = whatever
        serverPubKey    = getPubKey serverPK
        clientPubKey    = getPubKey clientPK
    in Script
             [OP_IF,
                 opPushData $ serialize serverPubKey, OP_CHECKSIGVERIFY,
             OP_ELSE,
                  encodeScriptInt $ lockTime, op_CHECKLOCKTIMEVERIFY, OP_DROP,
             OP_ENDIF,
             opPushData $ serialize clientPubKey, OP_CHECKSIG]



-- |scriptSig fulfilling 'paymentChannelRedeemScript' using two signatures (client+server)
paymentTxScriptSig :: PaymentSignature -> PaymentSignature -> Script --ScriptSig
paymentTxScriptSig clientSig serverSig = Script
    [opPushData $ serialize clientSig, --sig including SigHash byte
    opPushData $ serialize serverSig, --sig including SigHash byte
    OP_1]   -- Signal that we want to provide both PubKeys

-- |scriptSig fulfilling 'paymentChannelRedeemScript' using a single signature (client)
--  (not valid until specified lockTime)
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

getRedeemScript :: ChannelParameters -> Script
getRedeemScript (CChannelParameters senderPK recvrPK lockTime _) =
    paymentChannelRedeemScript senderPK recvrPK lockTime

getRedeemScriptBS :: ChannelParameters -> B.ByteString
getRedeemScriptBS = serialize . getRedeemScript

getP2SHInputScript :: ChannelParameters -> Script -> Script
getP2SHInputScript cp scriptSig =
    Script $ scriptOps scriptSig ++ redeemScript
        where
             redeemScript = [opPushData $ getRedeemScriptBS cp]

