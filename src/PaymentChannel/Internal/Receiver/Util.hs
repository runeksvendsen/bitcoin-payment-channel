module PaymentChannel.Internal.Receiver.Util
(
  module PaymentChannel.Internal.Receiver.Util
, module PaymentChannel.Internal.Receiver.Types
, module PaymentChannel.Internal.Class.Value
)
where

import PaymentChannel.Internal.Class.Value
import PaymentChannel.Internal.Receiver.Types
import qualified PaymentChannel.Internal.Settlement.Util as Settle
import PaymentChannel.Internal.Error
import PaymentChannel.Internal.Payment
import PaymentChannel.Internal.Metadata.Util
import PaymentChannel.Internal.Class.Value     (HasValue(..))
import qualified Network.Haskoin.Crypto as HC



updState :: ServerPayChanI a -> SignedPayment -> ServerPayChanI a
updState rpc p =
    rpc { rpcState = replacePayment (rpcState rpc) p }
  where
    replacePayment state p = state { pcsPayment = p }

-- |Create a 'ServerPayChanX', which has an associated BIP32 key index, from a
--  'ServerPayChan'
mkExtendedKeyRPC :: ServerPayChanI a -> HC.XPubKey -> Maybe ServerPayChanX
mkExtendedKeyRPC rpc@(MkServerPayChan pcs _) xpk =
    -- Check that it's the right pubkey first
    if xPubKey xpk == getPubKey (getRecvPubKey pcs) then
            Just $ mkExtendedKeyRPCUnsafe (fromIntegral $ HC.xPubIndex xpk) rpc
        else
            Nothing

mkExtendedKeyRPCUnsafe :: Word32 -> ServerPayChanI a -> ServerPayChanX
mkExtendedKeyRPCUnsafe i rpc =
    rpc {
        rpcMetadata =
            Metadata (fromIntegral i) 0 [] (valueOf $ rpcState rpc) ReadyForPayment
        }

mkDummyExtendedRPC :: ServerPayChanI a -> ServerPayChanX
mkDummyExtendedRPC = mkExtendedKeyRPCUnsafe 0

metaKeyIndex :: ServerPayChanI KeyDeriveIndex -> KeyDeriveIndex
metaKeyIndex = mdKeyData . rpcMetadata

-- | Server/receiver: set pubkey metadata
setMetadata :: a -> ServerPayChanI b -> ServerPayChanI a
setMetadata kd sp@MkServerPayChan{..} =
    sp { rpcMetadata =
            rpcMetadata { mdKeyData = kd }
       }


-- Status
setChannelStatus :: PayChanStatus -> ServerPayChanI a -> ServerPayChanI a
setChannelStatus s pcs@MkServerPayChan{ rpcMetadata = meta } =
    pcs { rpcMetadata = metaSetStatus s meta }

getChannelStatus :: ServerPayChanI a -> PayChanStatus
getChannelStatus MkServerPayChan{ rpcMetadata = meta } =
    metaGetStatus meta

markAsBusy :: ServerPayChanI a -> ServerPayChanI a
markAsBusy = setChannelStatus PaymentInProgress

isReadyForPayment :: ServerPayChanI a -> Bool
isReadyForPayment =
    (== ReadyForPayment) . getChannelStatus

checkChannelStatus :: ServerPayChanI a -> Either PayChanError (ServerPayChanI a)
checkChannelStatus rpc =
    maybe
    (Right rpc)
    (Left . StatusError)
    (checkReadyForPayment $ getChannelStatus rpc)


-- Metadata
calcNewData :: MetadataI a -> PayChanState BtcSig -> MetadataI a
calcNewData md@Metadata{ mdUnsettledValue = oldValRecvd, mdPayCount = oldCount } pcs =
    md { mdUnsettledValue = checkedVal, mdPayCount = oldCount+1 }
    where checkedVal  = if newValRecvd < oldValRecvd then error "BUG: Value lost :(" else newValRecvd
          newValRecvd = valueOf pcs

updateMetadata :: ServerPayChanI a -> ServerPayChanI a
updateMetadata rpc@MkServerPayChan{..} =
    rpc { rpcMetadata = calcNewData rpcMetadata rpcState }
