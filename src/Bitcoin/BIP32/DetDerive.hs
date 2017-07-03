module Bitcoin.BIP32.DetDerive
( module Bitcoin.BIP32.DetDerive
, module Bitcoin.BIP32.Types
)
where

import Bitcoin.Internal.Types
import Bitcoin.BIP32.Types
import Data.Serialize                     as Bin
import Data.Serialize.Get                 as BinGet
import qualified Network.Haskoin.Crypto   as HC
import Network.Haskoin.Crypto             (DerivPathI(..))


class DerivationSeed a where
    toDerivSeed :: a -> ByteString

-- | Deterministically derive a ChildKey from a sourceKey and a 'DerivationSeed'
detDerive
    :: forall a sourceKey t k derivPath.
    ( DerivationSeed a
    , DerivPathElem Word32 derivPath
    , IsChildKey sourceKey t k derivPath
    )
    => sourceKey
    -> a
    -> t k
detDerive sourceKey seed =
    mkChild sourceKey path
  where
    path :: derivPath
    path = toDerivPath (to128bitSeed seed)

to128bitSeed
    :: DerivationSeed a
    => a
    -> (Word32, Word32, Word32, Word32)
to128bitSeed a = either (\e -> error $ "to128bitSeed bug: " ++ e) id $
    BinGet.runGet getWords (Bin.encode . HC.hash256 . toDerivSeed $ a)
  where
    getWords = do
        w1 <- BinGet.getWord32be
        w2 <- BinGet.getWord32be
        w3 <- BinGet.getWord32be
        w4 <- BinGet.getWord32be
        return (w1,w2,w3,w4)

-- ########
-- ### Internal

--newtype KeyIndex = KeyIndex Word32


class DerivPathElem i derivPath where
    derivBegin :: i -> derivPath
    (/:|)  :: derivPath -> i -> derivPath

instance DerivPathElem Word32 HC.SoftPath where
    derivBegin i = Deriv :/ cap31Bit i
    (/:|) p i = p :/ cap31Bit i

instance DerivPathElem Word32 HC.HardPath where
    derivBegin i = Deriv :| cap31Bit i
    (/:|) p i = p :| cap31Bit i

cap31Bit :: Word32 -> Word32
cap31Bit = (`mod` (0x80000000 :: Word32))

class AsDerivePath t derivPath where
    toDerivPath :: t -> derivPath

instance DerivPathElem i derivPath => AsDerivePath (i, i, i, i) derivPath where
    toDerivPath (w1,w2,w3,w4) = derivBegin w1 /:| w2 /:| w3 /:| w4


