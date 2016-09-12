module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount where

import qualified Data.Serialize     as Ser
import qualified Data.Serialize.Put as SerPut
import qualified Data.Serialize.Get as SerGet
import qualified Data.Binary        as Bin
import qualified Data.Binary.Put    as BinPut
import qualified Data.Binary.Get    as BinGet
import           Data.Word


-- |Represents a bitcoin amount as number of satoshis.
--  1 satoshi = 1e-8 bitcoin.
--  Only natural numbers (>= 0) can be represented ('fromInteger' caps to a 'Word64').
--  It is thus not possible to construct a negative BitcoinAmount which, when added to
--  another BitcoinAmount, subtracts from its value. So we avoid the problem of being
--  able to invert subtraction/addition by supplying a negative value as one of the
--  arguments.
newtype BitcoinAmount = BitcoinAmount Integer
    deriving (Eq, Ord)
instance Show BitcoinAmount where
    show amount = show (toInteger amount) ++ " satoshi"

instance Num BitcoinAmount where
    (BitcoinAmount a1) * (BitcoinAmount a2) = BitcoinAmount (fromIntegral . capToWord64 $ a1*a2)
    (BitcoinAmount a1) + (BitcoinAmount a2) = BitcoinAmount (fromIntegral . capToWord64 $ a1+a2)
    (BitcoinAmount a1) - (BitcoinAmount a2) = BitcoinAmount (fromIntegral . capToWord64 $ a1-a2)
    abs = id    -- Always positive
    signum (BitcoinAmount 0) = BitcoinAmount 0
    signum (BitcoinAmount _) = BitcoinAmount 1
    fromInteger = BitcoinAmount . fromIntegral . capToWord64

instance Enum BitcoinAmount where
    toEnum = BitcoinAmount . fromIntegral . capToWord64 . fromIntegral
    fromEnum (BitcoinAmount amount) = fromIntegral amount

instance Real BitcoinAmount where
    toRational (BitcoinAmount amount) = toRational amount

instance Integral BitcoinAmount where
    toInteger (BitcoinAmount int) = int
    quotRem (BitcoinAmount _) (BitcoinAmount _) =
        error "Division of two BitcoinAmounts is undefined"

-- | Convert to 'Word64', with zero as floor, (maxBound :: Word64) as ceiling
capToWord64 :: Integer -> Word64
capToWord64 i = fromIntegral $
    max 0 cappedValue
        where
            cappedValue = min i $ fromIntegral (maxBound :: Word64)

instance Bin.Binary BitcoinAmount where
    put = BinPut.putWord64le . fromIntegral . toInteger
    get = BitcoinAmount . fromIntegral <$> BinGet.getWord64le

instance Ser.Serialize BitcoinAmount where
    put = SerPut.putWord64le . fromIntegral . toInteger
    get = BitcoinAmount . fromIntegral <$> SerGet.getWord64le
