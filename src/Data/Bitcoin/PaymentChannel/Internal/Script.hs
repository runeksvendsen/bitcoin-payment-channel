-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Data.Bitcoin.PaymentChannel.Internal.Script where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Util

import qualified  Network.Haskoin.Internals as HI
import qualified  Network.Haskoin.Util as HU
import qualified  Network.Haskoin.Crypto as HC
import Network.Haskoin.Script

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Bin
import Data.Word
import Data.Char as CH

valReceiverSigHash = SigAll True

getSigHashFlag :: BitcoinAmount -> SigHash
getSigHashFlag valueToSender =
    if valueToSender == 0 then
        SigNone True
    else
        SigSingle True

sigHashToByte :: SigHash -> Word8
sigHashToByte = fromIntegral . ord . head . C.unpack . serialize

-- |Generates OP_CHECKLOCKTIMEVERIFY redeemScript
paymentChannelRedeemScript :: HC.PubKey -> HC.PubKey -> Word32 -> Script
paymentChannelRedeemScript clientPK serverPK lockTime = Script
     [OP_IF,
         opPushData (serialize serverPK), OP_CHECKSIGVERIFY,
     OP_ELSE,
         encodeScriptInt lockTime, op_CHECKLOCKTIMEVERIFY, OP_DROP,
     OP_ENDIF,
     opPushData (serialize clientPK), OP_CHECKSIG]
         where encodeScriptInt i = opPushData $ B.pack $ HI.encodeInt (fromIntegral i)
            -- Note: HI.encodeInt encodes values up to and including 2^31-1 as 4 bytes
            --      and values 2^31 through 2^32-1 (upper limit) as 5 bytes.

-- |scriptSig fulfilling a payment channel redeemScript
paymentTxScriptSig :: PaymentSignature -> PaymentSignature -> Script --ScriptSig
paymentTxScriptSig clientSig serverSig = Script
    [opPushData $ serialize clientSig, --sig including SigHash byte
    opPushData $ serialize serverSig, --sig including SigHash byte
    OP_1]

-- |scriptSig used for the valueSender refund transaction
refundTxScriptSig :: HC.Signature -> Script
refundTxScriptSig clientSig = Script
    [opPushData (B.append (serialize clientSig) hashTypeByte),
    OP_0]
        where hashTypeByte = serialize (SigAll False)

-----Util-----

op_CHECKLOCKTIMEVERIFY = OP_NOP2

scriptToP2SHAddress :: Script -> HC.Address
scriptToP2SHAddress = HC.ScriptAddress . HC.hash160 . HC.getHash256 . HC.hash256 . serialize


getP2SHFundingAddress :: ChannelParameters -> HC.Address
getP2SHFundingAddress = scriptToP2SHAddress . getRedeemScript

getRedeemScript :: ChannelParameters -> Script --RedeemScript
getRedeemScript (CChannelParameters senderPK recvrPK lockTime) =
    paymentChannelRedeemScript senderPK recvrPK (toWord32 lockTime)

getRedeemScriptBS :: ChannelParameters -> B.ByteString
getRedeemScriptBS = serialize . getRedeemScript

getInputScript :: ChannelParameters -> Script -> Script
getInputScript cp scriptSig =
    Script $ scriptOps scriptSig ++ redeemScript
        where
             redeemScript = [opPushData $ getRedeemScriptBS cp]

