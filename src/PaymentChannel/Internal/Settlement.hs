{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module PaymentChannel.Internal.Settlement
-- (
--     createSignedSettlementTx
-- ,   settleReceivedValue
-- ,   UnsignedSettlementTx, mkClientSignedSettleTx
-- )
where

import PaymentChannel.Internal.Settlement.Util
import PaymentChannel.Internal.Receiver.Util
import PaymentChannel.Internal.Error.Internal


type SignedTx = BtcTx P2SH ChanParams PaymentScriptSig

-- |The value transmitted over the channel is settled when this transaction is in the Blockchain.
-- The receiver will want to make sure a transaction produced by this function
-- is included in a Bitcoin block before the refund transaction becomes valid (see 'getRefundBitcoinTx').
-- The sender can only close the channel before expiration by requesting this transaction
-- from the receiver and publishing it to the Bitcoin network.
getSignedSettlementTx :: forall signKey coi kd.
       DeriveChangeOut SignedPayment coi signKey ChanParams
    => ServerPayChanI kd
    -> coi
    -> SignM signKey (Either ReceiverError SignedTx)
getSignedSettlementTx rpc chgOut =
    fmapL SettleSigningError <$> signChangeTx (pcsPayment . rpcState $ rpc) chgOut







