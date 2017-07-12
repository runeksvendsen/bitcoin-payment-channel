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
    } deriving (Eq, Show)

type SettleTx = BtcTx P2SH ChanParams PaymentScriptSig

instance HasSigner BtcSig ChanParams where
    signerPubKey = Tagged . getPubKey . cpSenderPubKey
instance TransformSigData BtcSig () ChanParams where
    mkSigData _ = Tagged

instance HasSigner PaymentScriptSig ChanParams where
    signerPubKey = Tagged . getPubKey . cpReceiverPubKey
instance TransformSigData PaymentScriptSig BtcSig ChanParams where
    mkSigData oldSd = Tagged . PaymentScriptSig oldSd

-- Two signatures
instance SpendFulfillment PaymentScriptSig ChanParams where
    rawSigs PaymentScriptSig{..} ChanParams{..} =
        [ (getPubKey cpSenderPubKey, sssClientSig)
        , (getPubKey cpReceiverPubKey, sssServerSig) ]
    signatureScript PaymentScriptSig{..} _ = Script
        [ opPush sssClientSig
        , opPush sssServerSig
        , OP_1]   -- Make OP_IF in reeemScript evaluate to true (two pubkeys/sigs)

-- Single (client) signature
instance SpendFulfillment BtcSig ChanParams where
    rawSigs clientSig ChanParams{..} =
        [ (getPubKey cpSenderPubKey, clientSig) ]
    signatureScript _ _ = Script [] --TODO: separate rawSigs from signatureScript?


instance SpendCondition ChanParams where
    conditionScript = getRedeemScript
