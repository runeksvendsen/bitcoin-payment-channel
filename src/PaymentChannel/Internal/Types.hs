{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PaymentChannel.Internal.Types
( module PaymentChannel.Internal.Types
, RBPCP.SharedSecret
, fromInitialPayment
--, fromHash, toHash
, module X
, module Network.Haskoin.Transaction
, module Network.Haskoin.Crypto
, module Network.Haskoin.Script
, module Data.List.NonEmpty
, module Control.Monad.Time
, Word32, Word64, NFData
) where

import           Bitcoin.LockTime.Util                   as X
import           Bitcoin.SinglePair                      as X
import           Bitcoin.SpendCond.Cond                  as X
import           Bitcoin.Types                           as X
import           Control.DeepSeq                         (NFData)
import           PaymentChannel.Internal.ChanScript      as X
import           PaymentChannel.Internal.Config          as X
import           PaymentChannel.Internal.Crypto.PubKey   as X
import           PaymentChannel.Internal.Types.MonadConf as X
import           PaymentChannel.Internal.Util            as X
import qualified RBPCP.Types as RBPCP                    (SharedSecret(..))

import           Network.Haskoin.Transaction             hiding (signTx)

import           Network.Haskoin.Crypto                  hiding
                                                          (DerivPathI (..),
                                                          PubKey, hash160,
                                                          hash256)

import qualified Network.Haskoin.Crypto                  as HC
import           Network.Haskoin.Script
import qualified Network.Haskoin.Transaction             as HT

import           Control.Monad.Time
import qualified Data.ByteString.Base16                  as B16
import           Data.List.NonEmpty                      (NonEmpty (..))
import           Data.Maybe                              (fromMaybe)
import qualified Data.Serialize                          as Bin
import           Data.Word
import           GHC.Generics                            (Generic)



-- | The Bitcoin transaction script type we're using
--type ScriptType =  ChanParams
type Payment = SigSinglePair P2SH ChanParams
type SignedPayment   = Payment BtcSig
type UnsignedPayment = Payment ()

instance HasSendPubKey (Payment a) where getSendPubKey = getSendPubKey . pairRedeemScript
instance HasRecvPubKey (Payment a) where getRecvPubKey = getRecvPubKey . pairRedeemScript

instance HasLockTimeDate (Payment a) where
    getLockTimeDate = cpLockTime . pairRedeemScript

data PayChanState sigData = MkPayChanState
    { pcsPayment  :: Payment sigData
    -- | SHA256 hash of opening-channel-payment signature data ('BtcSig') (including 'SigHash' flag).
    --   Used as a shared client/server secret to prevent denial-of-service attacks.
    --   The resource, that the client delivers payments to, may be publicly
    --    accesible. Using this secret as part of the resource identifier for
    --    the channel makes it very hard for outsiders to guess valid resource
    --    identifiers from looking at in-blockchain data.
    , pcsSecret   :: RBPCP.SharedSecret
    -- |Various server-defined config options
    , pcsSettings :: ServerSettings
    } deriving (Eq, Show, Typeable, Generic, Serialize, ToJSON, FromJSON, NFData)

instance HasLockTimeDate (PayChanState a) where
    getLockTimeDate = getLockTimeDate . pcsPayment

--fromHash :: HC.Hash256 -> RBPCP.SharedSecret
--fromHash = SharedSecret

--toHash :: RBPCP.SharedSecret -> HC.Hash256
--toHash = ssHash

-- | Client/server shared secret is SHA256 hash of signature data (including SIGHASH flag)
--    of opening channel payment.
fromInitialPayment :: SigSinglePair t r BtcSig -> RBPCP.SharedSecret
fromInitialPayment  =
    RBPCP.SharedSecret . HC.hash256 . Bin.encode . getSigData

instance HasSendPubKey (PayChanState a) where getSendPubKey = getSendPubKey . pcsPayment
instance HasRecvPubKey (PayChanState a) where getRecvPubKey = getRecvPubKey . pcsPayment

instance HasSigData PayChanState where
    mapSigData f pcs@MkPayChanState{..} =
        pcs { pcsPayment =
                mapSigData f pcsPayment
            }

instance SetClientChangeAddr PayChanState where
    _setClientChangeAddr pcs@MkPayChanState{..} addr =
        pcs { pcsPayment =
                _setClientChangeAddr pcsPayment addr
            }

type EmptyClientPayChan = ClientPayChanI ()
type ClientPayChan = ClientPayChanI BtcSig

-- |State object for the value sender
data ClientPayChanI sigData = MkClientPayChan
    { -- |Internal state object
      spcState  :: PayChanState sigData
      -- |Payment-signing private key
    , spcPrvKey :: HC.PrvKeyC
    } deriving (Eq, Typeable, Generic, NFData)

instance HasSendPubKey (ClientPayChanI a) where getSendPubKey = getSendPubKey . spcState
instance HasRecvPubKey (ClientPayChanI a) where getRecvPubKey = getRecvPubKey . spcState

instance HasSigData ClientPayChanI where
    mapSigData f cpc@MkClientPayChan{..} =
           cpc { spcState =
                   mapSigData f spcState
               }

instance SetClientChangeAddr ClientPayChanI where
    _setClientChangeAddr cpc@MkClientPayChan{..} addr =
        cpc { spcState =
                _setClientChangeAddr spcState addr
            }

-- |Holds information about the Bitcoin transaction used to fund
-- the channel
data FundingTxInfo = CFundingTxInfo {
    ftiHash     ::  HT.TxHash,              -- ^ Hash of funding transaction.
    ftiOutIndex ::  Word32,                 -- ^ Index/"vout" of funding output (zero-based index of funding output within list of transaction outputs)
    ftiOutValue ::  NonDustyAmount      -- ^ Value of funding output (channel max value).
} deriving (Eq, Show, Typeable, Generic)



instance Serialize FundingTxInfo where
    put (CFundingTxInfo h idx val) =
        put h >> putWord32be idx >> put val
    get = CFundingTxInfo <$> get <*> getWord32be <*> get

instance ToJSON FundingTxInfo
instance FromJSON FundingTxInfo


-- instance ToJSON HC.Hash256 where
--    toJSON = String . cs . B16.encode . HC.getHash256

-- instance FromJSON HC.Hash256 where
--    parseJSON = withText "Hash256" (either fail return . decode . fst . B16.decode . cs)

instance Show ClientPayChan where
    show (MkClientPayChan s _) =
        "<ClientPayChanI:\n\t" ++ show s ++ ">"
