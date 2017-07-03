module PaymentChannel.Internal.Receiver.Util
( updState
, mkExtendedKeyRPC
--, mkDummyExtendedRPC
, metaKeyIndex
, setMetadata
, setChannelStatus
, getChannelStatus
, markAsBusy
, isReadyForPayment
, checkChannelStatus
, getClosingPayment
, calcNewData
, updateMetadata
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



updState :: ServerPayChanG kd sd -> SignedPayment -> ServerPayChanG kd BtcSig
updState rpc p =
    rpc { rpcState = replacePayment (rpcState rpc) p }
  where
    replacePayment state p' = state { pcsPayment = p' }

-- |Create a 'ServerPayChanX', which has an associated BIP32 extended pubkey, from a
--  'ServerPayChan'
mkExtendedKeyRPC :: ServerPayChanI kd -> ExtPub -> Maybe ServerPayChanX
mkExtendedKeyRPC rpc@(MkServerPayChan pcs _) xpk =
    -- Check that it's the right pubkey first
    if getKey xpk == getPubKey (getRecvPubKey pcs) then
            Just $ mkExtendedKeyRPCUnsafe xpk rpc
        else
            Nothing

mkExtendedKeyRPCUnsafe :: ExtPub -> ServerPayChanI kd -> ServerPayChanX
mkExtendedKeyRPCUnsafe xPub rpc =
    rpc {
        rpcMetadata =
            Metadata xPub 0 [] (valueOf $ rpcState rpc) ReadyForPayment
        }

--mkDummyExtendedRPC :: ServerPayChanI kd -> ServerPayChanX
--mkDummyExtendedRPC = mkExtendedKeyRPCUnsafe 0

metaKeyIndex :: ServerPayChanI KeyDeriveIndex -> KeyDeriveIndex
metaKeyIndex = mdKeyData . rpcMetadata

-- | Server/receiver: set pubkey metadata
setMetadata :: ServerPayChanG a sd -> b -> ServerPayChanG b sd
setMetadata sp@MkServerPayChan{..} kd =
    sp { rpcMetadata =
            rpcMetadata { mdKeyData = kd }
       }


-- Status
setChannelStatus :: PayChanStatus -> ServerPayChanG kd sd -> ServerPayChanG kd sd
setChannelStatus s pcs@MkServerPayChan{ rpcMetadata = meta } =
    pcs { rpcMetadata = metaSetStatus s meta }

getChannelStatus :: ServerPayChanG kd sd -> PayChanStatus
getChannelStatus MkServerPayChan{ rpcMetadata = meta } =
    metaGetStatus meta

markAsBusy :: ServerPayChanG kd sd -> ServerPayChanG kd sd
markAsBusy = setChannelStatus PaymentInProgress

isReadyForPayment :: ServerPayChanG kd sd -> Bool
isReadyForPayment =
    (== ReadyForPayment) . getChannelStatus

checkChannelStatus :: ServerPayChanG kd sd -> Either PayChanError (ServerPayChanG kd sd)
checkChannelStatus rpc =
    maybe
    (Right rpc)
    (Left . StatusError)
    (checkReadyForPayment $ getChannelStatus rpc)

getClosingPayment :: ServerPayChanI kd -> Maybe SignedPayment
getClosingPayment spc =
    case getChannelStatus spc of
        ChannelClosed p -> Just p
        _               -> Nothing

-- Metadata
calcNewData :: MetadataI a -> PayChanState BtcSig -> MetadataI a
calcNewData md@Metadata{ mdUnsettledValue = oldValRecvd, mdPayCount = oldCount } pcs =
    md { mdUnsettledValue = checkedVal, mdPayCount = oldCount+1 }
    where checkedVal  = if newValRecvd < oldValRecvd then error "BUG: Value lost :(" else newValRecvd
          newValRecvd = valueOf pcs

updateMetadata :: ServerPayChanI kd -> ServerPayChanI kd
updateMetadata rpc@MkServerPayChan{..} =
    rpc { rpcMetadata = calcNewData rpcMetadata rpcState }


