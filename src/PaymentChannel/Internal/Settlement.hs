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
import qualified Data.List.NonEmpty     as NE


type SignedTx = BtcTx P2SH ChanParams PaymentScriptSig

mkClientSignedSettleTx ::
       NE.NonEmpty (ServerPayChanI kd)
    -> [BtcOut]
    -> ClientSignedTx
mkClientSignedSettleTx rpcL extraOuts =
    txAddOuts extraOuts $ toClientSignedTx payLst
        where payLst = NE.map (pcsPayment . rpcState) rpcL

getSignedSettlementTx ::
       -- (MonadSign m signKey, MaybeKeyDeriveIndex kd)
       HasSigningKey key P2SH ChanParams BtcSig =>
       ServerPayChanI kd
    -> ChangeOut
    -> SignM key (Either ReceiverError SignedTx)
getSignedSettlementTx rpc chgOut =
    fmapL SettleSigningError <$> signChangeTx settleTx chgOut
  where
    settleTx = mkClientSignedSettleTx (rpc :| []) []






