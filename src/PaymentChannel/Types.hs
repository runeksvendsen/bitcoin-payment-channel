{-|
Module      : PaymentChannel.Types
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com

Types used with the interface provided by "PaymentChannel".

-}

module PaymentChannel.Types
(
    -- *Shared sender/receiver types/functions
    FundingTxInfo(..)
  , ChanParams(..)
  , PaymentChannel(..), PayChan
  , PaymentChannelRecv(..)
  , SharedSecret, HasSharedSecret(..) --, fromHash, toHash
  , fundingAddress
  , clientChangeAddress
  , getFundingAmount

    -- *Sender
  , ClientPayChanI(..)

    -- *Receiver
  , ServerPayChan, ServerPayChanG(rpcMetadata)
  , PayChanStatus(..), MetadataI(..), OpenError(..)
  , S.getChannelStatus, S.setChannelStatus
  , S.markAsBusy, S.isReadyForPayment
  , ServerSettings(..), Hour(..)
  
    -- *Receiver state (with pubkey metadata)
  , ServerPayChanX
  , S.mkExtendedDerivRpc
  , Script.UserParams(..)
  , Script.deriveRecvPub

    -- *Payment
  , SignedPayment

    -- *Receiver settlement
  , ClosedServerChanI, ClosedServerChan, ClosedServerChanX
  , getClosedState, cscClosingPayment
  , SettleTx

    -- **Error
  , PayChanError(..), IsPayChanError(..)

    -- *Bitcoin
  , module X
  , module Bitcoin.Fee
  , PaymentValueSpec(..)
  , Capped(..)

    -- *Crypto
  , SendPubKey(..),RecvPubKey(..),IsPubKey(..),HasSendPubKey(..),HasRecvPubKey(..)

    -- *Util
  , module Bitcoin.SpendCond.Util
  , fromDate
  , getChanState
  , clientChangeVal
  , toHaskoinTx
)
where

import PaymentChannel.Internal.Types
import PaymentChannel.Internal.Receiver.Types
import PaymentChannel.Internal.Metadata.Util
import PaymentChannel.Internal.Serialization ()
import PaymentChannel.Internal.Class.Value     (HasValue(..))
import PaymentChannel.Internal.Receiver.Open   (OpenError(..))
import Bitcoin.SpendCond.Util
import Bitcoin.Types                  as X hiding (fromDate)
import Bitcoin.Conversion (toHaskoinTx)

import qualified PaymentChannel.Internal.Receiver.Util as S
import qualified PaymentChannel.Internal.ChanScript as Script
import PaymentChannel.Internal.Error 
import Bitcoin.Fee

import qualified  Network.Haskoin.Crypto as HC


-- |Both sender and receiver state objects have a 'PaymentChannelState'
class HasSignedPayChanState a where
    getChanState :: a -> PayChanState BtcSig

instance HasSignedPayChanState ClientPayChan where
    getChanState = spcState

instance HasSignedPayChanState (ServerPayChanI s) where
    getChanState = rpcState
    

class HasPayChanState a where
    getPayChanState :: a -> PayChanState ()

instance HasPayChanState (ClientPayChanI a) where
    getPayChanState = mapSigData (const ()) . spcState

instance HasPayChanState (ServerPayChanG kd a) where
    getPayChanState = mapSigData (const ()) . rpcState
    

getFundingAmount  :: HasPayChanState a => a -> BtcAmount
getFundingAmount = fundingValue . pcsPayment . getPayChanState

fundingAddress :: HasPayChanState a => a -> HC.Address
fundingAddress = Script.getP2SHFundingAddress . pairRedeemScript . pcsPayment . getPayChanState

clientChangeAddress :: HasPayChanState a => a -> HC.Address
clientChangeAddress = clientChangeAddr . pcsPayment . getPayChanState


-- |Get various information about an open payment channel.
class HasSignedPayChanState a => PaymentChannel a where
    -- |Get amount received by receiver/left for sender
    valueToMe :: a -> BtcAmount
    -- |Remaining channel value
    channelValueLeft :: a -> BtcAmount
    -- |For internal use
    getStatePayment :: a -> SignedPayment

instance PaymentChannel ClientPayChan where
    valueToMe = clientChangeVal . pcsPayment . spcState
    channelValueLeft = valueToMe
    getStatePayment = pcsPayment . spcState

instance PaymentChannel (ServerPayChanI s) where
    valueToMe (MkServerPayChan s _) = valueOf s
    channelValueLeft = clientChangeVal . getStatePayment
    getStatePayment = pcsPayment . rpcState

-- |Payment channel state objects with metadata information
class PaymentChannel a => PaymentChannelRecv a where
    -- |Get total amount (both settled and unsettled) sent by client
    clientTotalValueSent :: a -> BtcAmount

instance PaymentChannelRecv (ServerPayChanI (MetadataI a)) where
    clientTotalValueSent = metaTotalValXfer . rpcMetadata


class HasSharedSecret a where
    getSecret :: a -> SharedSecret

instance HasSharedSecret (PayChanState a)       where getSecret = pcsSecret
instance HasSharedSecret (ServerPayChanG sd s)  where getSecret = getSecret . rpcState
instance HasSharedSecret ClientPayChan          where getSecret = getSecret . spcState

-- |Short-hand
class PaymentChannel a => PayChan a


-- | Capped/non-capped amount-specifications (get value)
class PaymentValueSpec val where
    paymentValue :: BtcAmount           -- ^ Available value
                 -> ServerSettings
                 -> val                 -- ^ Payment value spec
                 -> BtcAmount           -- ^ Actual payment amount

---- | Capped/non-capped amount-specifications (make return type)
--class PaymentValueSpec val => PaymentValueRet val ret | ret -> val where
--    mkReturnVal  :: Tagged val BtcAmount            -- ^ Actual payment amount
--                 -> Either BtcError SignedPayment   -- ^ createPayment return value
--                 -> ret                             -- ^ 'val'-specific return type

instance PaymentValueSpec BtcAmount where
    paymentValue _ = const id

instance PaymentValueSpec (Capped BtcAmount) where
    paymentValue valueAvailable ServerSettings{..} (Capped amt) =
        if amt >= valueAvailable
            then valueAvailable
            else min amt (valueAvailable - serverConfDustLimit)
