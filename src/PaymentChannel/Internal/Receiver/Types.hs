{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module PaymentChannel.Internal.Receiver.Types
(
  module PaymentChannel.Internal.Receiver.Types
, module PaymentChannel.Internal.Types
, module PaymentChannel.Internal.State
)
where

import PaymentChannel.Internal.State
import PaymentChannel.Internal.Types
import PaymentChannel.Internal.Metadata.Types
import PaymentChannel.Internal.Util


-- |ServerPayChan without receiver key metadata
type ServerPayChan = ServerPayChanI ()
-- |ServerPayChan with BIP32, "extended key" index as metadata
type ServerPayChanX = ServerPayChanI KeyDeriveIndex

-- |State object for the value receiver. "kd" is used to store
--  information about the receiver key(s) used for this state object.
data ServerPayChanI kd = MkServerPayChan {
    -- |Internal state object
    rpcState    :: PayChanState BtcSig
  , rpcMetadata :: MetadataI kd
} deriving (Eq, Show, Typeable, Generic, Serialize, NFData)

instance HasSendPubKey (ServerPayChanI a) where getSendPubKey = getSendPubKey . rpcState
instance HasRecvPubKey (ServerPayChanI a) where getRecvPubKey = getRecvPubKey . rpcState



instance ToJSON d => ToJSON (ServerPayChanI d) where
    toJSON rpc = object
        [ "state"       .= rpcState rpc
        , "metadata"    .= rpcMetadata rpc
        ]

instance FromJSON d => FromJSON (ServerPayChanI d) where
    parseJSON = withObject "ServerPayChanI" $ \o ->
        MkServerPayChan <$>
            o .: "state" <*>
            o .: "metadata"


