{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module PaymentChannel.Internal.Types
(
    module PaymentChannel.Internal.Types
  , module X
  , module Network.Haskoin.Transaction
  , module Network.Haskoin.Crypto
  , module Network.Haskoin.Script
  , module Data.List.NonEmpty
  , module Control.Monad.Time
  , Word32, Word64
) where

import PaymentChannel.Internal.Config           as X
import PaymentChannel.Internal.Util             as X
import Bitcoin.Types    as X
import PaymentChannel.Internal.ChanScript       as X
import PaymentChannel.Internal.Crypto.PubKey    as X
import Bitcoin.SinglePair as X
import Bitcoin.SpendCond.Cond as X
import Bitcoin.LockTime.Util as X



import           Network.Haskoin.Transaction hiding (signTx)

import           Network.Haskoin.Crypto hiding (DerivPathI(..), PubKey, hash160, hash256)

import           Network.Haskoin.Script
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Word
import           Data.List.NonEmpty         (NonEmpty(..))
import           GHC.Generics               (Generic)
import           Data.Maybe                 (fromMaybe)
import Control.Monad.Time

type Payment = SigSinglePair (P2SH ChanParams)
type SignedPayment   = Payment BtcSig
type UnsignedPayment = Payment ()

instance HasSendPubKey (Payment a) where getSendPubKey = getSendPubKey . pairRedeemScript
instance HasRecvPubKey (Payment a) where getRecvPubKey = getRecvPubKey . pairRedeemScript

instance HasLockTimeDate (Payment a) where
    getLockTimeDate = cpLockTime . pairRedeemScript

data PayChanState sigData = MkPayChanState
    { pcsPayment    :: Payment sigData
    -- | SHA256 hash signature data  of the opening channel payment (not including 'SigHash' flag).
    --   Used as a shared client/server secret to
    --    prevent denial-of-service attacks.
    --   The resource,
    --    that the client delivers payments to, may be publicly
    --    accesible. Using this secret as part of the resource
    --    identifier for the channel makes it very hard for outsiders
    --    to guess valid resource identifiers, from looking at
    --    in-blockchain data.
    , pcsToken      :: HC.Hash256
    } deriving (Eq, Show, Typeable, Generic, Serialize, ToJSON, FromJSON, NFData)

instance HasSendPubKey (PayChanState a) where getSendPubKey = getSendPubKey . pcsPayment
instance HasRecvPubKey (PayChanState a) where getRecvPubKey = getRecvPubKey . pcsPayment

type EmptyClientPayChan = ClientPayChanI ()
type ClientPayChan = ClientPayChanI BtcSig

-- |State object for the value sender
data ClientPayChanI sigData = MkClientPayChan
    { -- |Internal state object
      spcState    :: PayChanState sigData
    , -- |Payment-signing function
      spcPrvKey   :: HC.PrvKeyC
    } deriving (Eq, Typeable, Generic, NFData)

instance HasSendPubKey (ClientPayChanI a) where getSendPubKey = getSendPubKey . spcState
instance HasRecvPubKey (ClientPayChanI a) where getRecvPubKey = getRecvPubKey . spcState

-- |Holds information about the Bitcoin transaction used to fund
-- the channel
data FundingTxInfo = CFundingTxInfo {
    ftiHash         ::  HT.TxHash,              -- ^ Hash of funding transaction.
    ftiOutIndex     ::  Word32,                 -- ^ Index/"vout" of funding output (zero-based index of funding output within list of transaction outputs)
    ftiOutValue     ::  NonDusty BtcAmount      -- ^ Value of funding output (channel max value).
} deriving (Eq, Show, Typeable, Generic)



instance Serialize FundingTxInfo where
    put (CFundingTxInfo h idx val) =
        put h >> putWord32be idx >> put val
    get = CFundingTxInfo <$> get <*> getWord32be <*> get

instance ToJSON FundingTxInfo
instance FromJSON FundingTxInfo

ftiOutPoint :: FundingTxInfo -> HT.OutPoint
ftiOutPoint CFundingTxInfo{..} = HT.OutPoint ftiHash ftiOutIndex

instance ToJSON HC.Hash256 where
    toJSON = String . cs . B16.encode . HC.getHash256

instance FromJSON HC.Hash256 where
    parseJSON = withText "Hash256" (either fail return . decode . fst . B16.decode . cs)

instance Show ClientPayChan where
    show (MkClientPayChan s _) =
        "<ClientPayChanI:\n\t" ++ show s ++ ">"
