{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module PaymentChannel.Internal.Receiver.Types
(
  module PaymentChannel.Internal.Receiver.Types
, module X
, SettleTx
)
where

import PaymentChannel.Internal.Payment.Types            (SettleTx)
import PaymentChannel.Internal.Types                    as X
import PaymentChannel.Internal.Metadata.Types           as X
import PaymentChannel.Internal.Types.MonadConf          as X


-- |ServerPayChan without receiver key metadata
type ServerPayChan = ServerPayChanI ()
-- |ServerPayChan with BIP32, "extended key" index as metadata
type ServerPayChanX = ServerPayChanI ExtPub

-- |State object for the value receiver. "kd" is used to store
--   information about the receiver key(s) used for this state object.
data ServerPayChanG kd sd = MkServerPayChan {
    -- |Internal state object
    rpcState    :: PayChanState sd
  , rpcMetadata :: MetadataI kd
} deriving (Eq, Show, Typeable, Generic, Serialize, NFData)

instance HasLockTimeDate (ServerPayChanG kd sd) where
    getLockTimeDate = getLockTimeDate . rpcState

type ServerPayChanI kd = ServerPayChanG kd BtcSig

dummyFromClientState :: ClientPayChan -> ServerPayChanI ()
dummyFromClientState MkClientPayChan{..} =
    MkServerPayChan spcState initialMetadata


data ClosedServerChanI kd = MkClosedServerChan
    { cscState          :: ServerPayChanG kd BtcSig
    , cscClosingPayment :: SignedPayment
    } deriving (Eq, Show, Typeable, Generic, Serialize, NFData)

type ClosedServerChan = ClosedServerChanI ()
type ClosedServerChanX = ClosedServerChanI ExtPub

getClosedState :: ClosedServerChanI kd -> ServerPayChanI kd
getClosedState = cscState

getClosedPayment :: ClosedServerChanI kd -> SignedPayment
getClosedPayment = cscClosingPayment

instance HasSendPubKey (ServerPayChanG kd sd) where getSendPubKey = getSendPubKey . rpcState
instance HasRecvPubKey (ServerPayChanG kd sd) where getRecvPubKey = getRecvPubKey . rpcState

instance HasSigData (ServerPayChanG kd) where
    mapSigData f spc@MkServerPayChan{..} =
         spc { rpcState =
                 mapSigData f rpcState
             }

instance SetClientChangeAddr (ServerPayChanG kd) where
    _setClientChangeAddr spc@MkServerPayChan{..} addr =
        spc { rpcState =
                _setClientChangeAddr rpcState addr
            }

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

