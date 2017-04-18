{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveAnyClass #-}
module PaymentChannel.Internal.Receiver.Key
( RootKey
, fromRootPrv
, External
, Internal
, PairPub(..)
, ChildPair
, pairPub
, ChildPub
, fromPair
, IsChildKey(..)
, HasSubKey(..)
)
where

import PaymentChannel.Internal.Crypto.PubKey
import Data.Word
import PaymentChannel.Internal.Types          hiding (NonEmpty((:|)))
import qualified Network.Haskoin.Crypto       as HC
import           Network.Haskoin.Crypto       (DerivPathI(..))


-- | Source of all other key types
newtype RootKey = RootKey HC.XPrvKey deriving (Generic, NFData, Serialize, FromJSON, ToJSON)

fromRootPrv = RootKey
rootPrv (RootKey k) = k

newtype External a = External { getExternal :: a } deriving (Functor, Generic, NFData)
newtype Internal a = Internal { getInternal :: a } deriving (Functor, Generic, NFData)

data    ChildPair = ChildPair { pairPriv  :: HC.XPrvKey
                              , pairPub'   :: HC.XPubKey } deriving (Generic, NFData)

fromChildPrv :: HC.XPrvKey -> ChildPair
fromChildPrv prv = ChildPair prv (HC.deriveXPubKey prv)



newtype ChildPub  = ChildPub  { pubPub    :: HC.XPubKey }

fromPair :: ChildPair -> ChildPub
fromPair = ChildPub . pairPub'

class PairPub a where
    pairPub :: a -> HC.XPubKey
instance PairPub (External ChildPair) where   pairPub = pairPub' . getExternal
instance PairPub (Internal ChildPair) where   pairPub = pairPub' . getInternal


class IsChildKey (t :: * -> *) k where
    mkChild :: RootKey -> t k

instance IsChildKey Internal ChildPair  where
    mkChild (RootKey k) = Internal $
        fromChildPrv (HC.derivePath internalKeyPath k)
            where internalKeyPath :: HC.HardPath
                  internalKeyPath = Deriv :| 1

instance IsChildKey External ChildPair  where
    mkChild (RootKey k) = External $
        fromChildPrv (HC.derivePath externalKeyPath k)
            where externalKeyPath :: HC.SoftPath
                  externalKeyPath = Deriv :/ 0

instance IsChildKey External ChildPub where
    mkChild = fmap fromPair . mkChild


class IsChildKey t k => HasSubKey t k sub where
    subKey  :: t k -> KeyDeriveIndex -> sub

-- Internal ChildPair
instance HasSubKey Internal ChildPair (HC.PrvKeyC, HC.XPubKey) where
    subKey (Internal (ChildPair k _)) i = fromPrvSub prvSub
        where prvSub = HC.hardSubKey k (word31Index i)

instance HasSubKey Internal ChildPair HC.PrvKeyC where
    subKey pair i = fst (subKey pair i :: (HC.PrvKeyC, HC.XPubKey))

instance HasSubKey Internal ChildPair HC.XPubKey where
    subKey pair i = snd (subKey pair i :: (HC.PrvKeyC, HC.XPubKey))

instance HasSubKey Internal ChildPair HC.PubKeyC where
    subKey pair = HC.xPubKey . subKey pair

-- External ChildPair
instance HasSubKey External ChildPair (HC.PrvKeyC, HC.XPubKey) where
    subKey (External (ChildPair k _)) i = fromPrvSub prvSub
        where prvSub = HC.prvSubKey k (word31Index i)

instance HasSubKey External ChildPair HC.PrvKeyC where
    subKey pair i = fst (subKey pair i :: (HC.PrvKeyC, HC.XPubKey))

instance HasSubKey External ChildPair HC.XPubKey where
    subKey pair i = snd (subKey pair i :: (HC.PrvKeyC, HC.XPubKey))

instance HasSubKey External ChildPair HC.PubKeyC where
    subKey pair = HC.xPubKey . subKey pair

-- External ChildPub
instance HasSubKey External ChildPub HC.XPubKey where
    subKey (External (ChildPub pk)) i =
        HC.pubSubKey pk (word31Index i)

instance HasSubKey External ChildPub HC.PubKeyC where
    subKey pub = HC.xPubKey . subKey pub


fromPrvSub :: HC.XPrvKey -> (HC.PrvKeyC, HC.XPubKey)
fromPrvSub prvSub =
    (HC.xPrvKey prvSub, HC.deriveXPubKey prvSub)


mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)
