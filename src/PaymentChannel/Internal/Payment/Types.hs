module PaymentChannel.Internal.Payment.Types
(
  module PaymentChannel.Internal.Payment.Types
, module PaymentChannel.Internal.Types
, module Bitcoin.SpendCond.Cond
)
where

import PaymentChannel.Internal.Types
import Bitcoin.Types
import Bitcoin.Util
import Bitcoin.SinglePair
import Bitcoin.SpendCond.Cond
import Bitcoin.Signature

import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import qualified Network.Haskoin.Script         as HS
import qualified Data.Ord                       as Ord



-- | scriptInput for a settlement transaction using client+server pubkey
data PaymentScriptSig = PaymentScriptSig
    { sssClientSig   :: BtcSig
    , sssServerSig   :: BtcSig
    }

instance TransformSigData BtcSig () ChanParams where
    mkSigData _ btcSig _ = btcSig

instance TransformSigData PaymentScriptSig BtcSig ChanParams where
    mkSigData clientSig serverSig _ = PaymentScriptSig clientSig serverSig

-- Two signatures
instance SpendFulfillment PaymentScriptSig ChanParams where
    rawSigs PaymentScriptSig{..} MkChanParams{..} =
        [ (getPubKey cpReceiverPubKey, sssClientSig)
        , (getPubKey cpSenderPubKey  , sssServerSig) ]
    signatureScript PaymentScriptSig{..} _ = Script
        [ opPush sssClientSig
        , opPush sssServerSig
        , OP_1]   -- Make OP_IF in reeemScript evaluate to true (two pubkeys/sigs)

-- Single (client) signature
instance SpendFulfillment BtcSig ChanParams where
    rawSigs clientSig MkChanParams{..} =
        [ (getPubKey cpSenderPubKey, clientSig) ]
    signatureScript _ _ = Script [] --TODO: separate rawSigs from signatureScript?


instance SpendCondition ChanParams where
    conditionScript = getRedeemScript
