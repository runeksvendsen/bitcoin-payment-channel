module Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
(
  module Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
, module Data.Bitcoin.PaymentChannel.Internal.State
)
where

import Data.Bitcoin.PaymentChannel.Internal.State
import Data.Bitcoin.PaymentChannel.Internal.Metadata.Types
import Data.Bitcoin.PaymentChannel.Internal.Util


-- |ReceiverPaymentChannel without receiver key metadata
type ReceiverPaymentChannel = ReceiverPaymentChannelI ()
-- |ReceiverPaymentChannel with BIP32, "extended key" index as metadata
type ReceiverPaymentChannelX = ReceiverPaymentChannelI KeyDeriveIndex

-- |State object for the value receiver. "kd" is used to store
--  information about the receiver key(s) used for this state object.
data ReceiverPaymentChannelI kd = CReceiverPaymentChannel {
    -- |Internal state object
    rpcState    :: PaymentChannelState
  , rpcMetadata :: MetadataI kd
} deriving (Eq, Typeable)

instance HasSendPubKey (ReceiverPaymentChannelI a) where
    getSendPubKey = getSendPubKey . rpcState

instance HasRecvPubKey (ReceiverPaymentChannelI a) where
    getRecvPubKey = getRecvPubKey . rpcState

-- |Short-hands
type RecvPayChanI   = ReceiverPaymentChannelI
type RecvPayChan    = ReceiverPaymentChannel
type RecvPayChanX = ReceiverPaymentChannelI KeyDeriveIndex

instance Serialize a => Serialize (ReceiverPaymentChannelI a) where
    put (CReceiverPaymentChannel rpc pki ) =
        put rpc >> putWord8 0x02 >> put pki
    get = CReceiverPaymentChannel <$> get <*> get



instance ToJSON d => ToJSON (ReceiverPaymentChannelI d) where
    toJSON rpc = object
        [ "state"       .= rpcState rpc
        , "metadata"    .= rpcMetadata rpc
        ]

instance FromJSON d => FromJSON (ReceiverPaymentChannelI d) where
    parseJSON = withObject "ReceiverPaymentChannelI" $ \o ->
        CReceiverPaymentChannel <$>
            o .: "state" <*>
            o .: "metadata"


instance Show ReceiverPaymentChannel where
    show (CReceiverPaymentChannel s _) =
        "<ReceiverPaymentChannel:\n\t" ++ show s ++ ">"

instance Show ReceiverPaymentChannelX where
    show (CReceiverPaymentChannel s m) =
        "<ReceiverPaymentChannelX:\n\t" ++ show s ++
        show m ++ ">"

