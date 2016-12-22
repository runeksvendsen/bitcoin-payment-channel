module Data.Bitcoin.PaymentChannel.Internal.Receiver.Util
(
  module Data.Bitcoin.PaymentChannel.Internal.Receiver.Util
, module Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
)
where

import Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
import qualified Data.Bitcoin.PaymentChannel.Internal.Settlement.Util as Settle
import Data.Bitcoin.PaymentChannel.Internal.Error
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.Metadata.Util
import qualified Network.Haskoin.Crypto as HC


cPaymentIdxFromState :: ReceiverPaymentChannelI a -> ClientSignedPaymentI a
cPaymentIdxFromState CReceiverPaymentChannel{..} =
    csp { csKeyData = mdKeyData rpcMetadata }
        where csp = cPaymentFromState rpcState


-- |Create a 'ReceiverPaymentChannelX', which has an associated XPubKey, from a
--  'ReceiverPaymentChannel'
mkExtendedKeyRPC :: RecvPayChanI a -> HC.XPubKey -> Maybe RecvPayChanX
mkExtendedKeyRPC (CReceiverPaymentChannel pcs _) xpk =
    -- Check that it's the right pubkey first
    if xPubKey xpk == getPubKey (pcsServerPubKey pcs) then
            Just $ CReceiverPaymentChannel pcs $
                Metadata (fromIntegral $ HC.xPubIndex xpk) [] (pcsValueTransferred pcs) ReadyForPayment
        else
            Nothing



setChannelStatus :: PayChanStatus -> RecvPayChanI a -> RecvPayChanI a
setChannelStatus s pcs@CReceiverPaymentChannel{ rpcMetadata = meta } =
    pcs { rpcMetadata = metaSetStatus s meta }

getChannelStatus :: RecvPayChanI a -> PayChanStatus
getChannelStatus CReceiverPaymentChannel{ rpcMetadata = meta } =
    metaGetStatus meta

metaKeyIndex :: RecvPayChanI KeyDeriveIndex -> KeyDeriveIndex
metaKeyIndex = mdKeyData . rpcMetadata


newMetaAfterSettle :: RecvPayChanI a -> MetadataI a
newMetaAfterSettle rpc@CReceiverPaymentChannel{rpcMetadata = prevMd} =
    prevMd
        { mdSettledValue   = Settle.fromState rpc : mdSettledValue prevMd
        , mdUnsettledValue = 0
        , mdChannelStatus  = statusAfterSettlement rpc
        }


-- |What would the status of the payment channel be
--   if the settlement transaction were published right now?
statusAfterSettlement :: ReceiverPaymentChannelI a -> PayChanStatus
statusAfterSettlement rpc =
    if changeAddress rpc == fundingAddress rpc then
        ReadyForPayment
    else
        ChanClosed
  where
    changeAddress = pcsClientChangeAddress . rpcState
    fundingAddress = getP2SHFundingAddress . pcsParameters . rpcState



markAsBusy :: RecvPayChanI a -> RecvPayChanI a
markAsBusy = setChannelStatus PaymentInProgress

isReadyForPayment :: RecvPayChanI a -> Bool
isReadyForPayment =
    (== ReadyForPayment) . getChannelStatus


calcNewData :: MetadataI a -> PaymentChannelState -> MetadataI a
calcNewData md@Metadata{ mdUnsettledValue = oldValRecvd } pcs =
    md { mdUnsettledValue = checkedVal }
    where checkedVal  = if newValRecvd < oldValRecvd then error "BUG: Value lost :(" else newValRecvd
          newValRecvd = pcsValueTransferred pcs

checkChannelStatus :: ReceiverPaymentChannelI a -> Either PayChanError (ReceiverPaymentChannelI a)
checkChannelStatus rpc =
    maybe
    (Right rpc)
    (Left . StatusError)
    (checkReadyForPayment $ getChannelStatus rpc)


updateWithMetadata :: MetadataI a -> PaymentChannelState -> ReceiverPaymentChannelI a
updateWithMetadata oldData pcs =
    CReceiverPaymentChannel pcs (calcNewData oldData pcs)
