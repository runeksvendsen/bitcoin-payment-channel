module Data.Bitcoin.PaymentChannel.Internal.Payment.Types
(
  module Data.Bitcoin.PaymentChannel.Internal.Payment.Types
, module Data.Bitcoin.PaymentChannel.Internal.Types
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import qualified Data.Ord as Ord

-- |Represents a P2SH ANYONECANPAY input/output pair from a payment channel.
data UnsignedPayment = UnsignedPayment
  {  fundingOutPoint    :: HT.OutPoint
  ,  chanFundingValue   :: BitcoinAmount
  ,  upChannelParams    :: ChannelParameters
  ,  changeAddress      :: HC.Address
  ,  changeValue        :: BitcoinAmount
  } deriving (Eq, Show)

-- |Contains necessary data, except receiver signature, to construct a settlement transaction.
data ClientSignedPaymentI kd = ClientSignedPayment
  {  cspPayment :: UnsignedPayment
  ,  clientSig  :: PaymentSignature
  ,  csKeyData  :: kd
  } deriving (Eq, Show)

type ClientSignedPayment = ClientSignedPaymentI ()
type ClientSignedPaymentIdx = ClientSignedPaymentI KeyDeriveIndex
type ClientSignedPaymentPrv = ClientSignedPaymentI HC.PrvKeyC


-- | SigSingle payment go first, then SigNone, then SigAll
instance (Eq a, Show a) => Ord.Ord (ClientSignedPaymentI a) where
    compare p1 p2 = let sigHash = psSigHash . clientSig in
            case (sigHash p1, sigHash p2) of
                (HS.SigSingle  _,               _) -> Ord.LT
                (HS.SigNone    _, HS.SigSingle  _) -> Ord.GT
                (HS.SigNone    _, HS.SigNone    _) -> Ord.EQ
                (HS.SigNone    _, HS.SigAll     _) -> Ord.LT
                (HS.SigAll     _,               _) -> Ord.GT
                (HS.SigUnknown{},               _) ->
                    error $ "Bad SigHash in ClientSignedPayment: " ++ show p1
                (              _, HS.SigUnknown{}) ->
                    error $ "Bad SigHash in ClientSignedPayment: " ++ show p2



