module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount where

import qualified Data.Serialize     as Ser
import qualified Data.Serialize.Put as SerPut
import qualified Data.Serialize.Get as SerGet
import           Data.Word
import           Data.Ratio


-- |Represents a bitcoin amount as number of satoshis.
--  1 satoshi = 1e-8 bitcoin. 1e8 satohis = 1 bitcoin.
--  Only amounts >= 0 can be represented, and 'fromInteger' caps to a 'Word64'.
--  It is thus not possible to eg. construct a negative BitcoinAmount which, when added to
--  another BitcoinAmount, subtracts from its value. Adding two large amounts together will
--  never overflow, nor will subtraction underflow.
newtype BitcoinAmount = BitcoinAmount Integer
    deriving (Eq, Ord)
instance Show BitcoinAmount where
    show amount = show (toInteger amount) ++ " satoshi"

instance Num BitcoinAmount where
    (BitcoinAmount a1) * (BitcoinAmount a2) = BitcoinAmount (fromIntegral . capTo21Mill $ a1*a2)
    (BitcoinAmount a1) + (BitcoinAmount a2) = BitcoinAmount (fromIntegral . capTo21Mill $ a1+a2)
    (BitcoinAmount a1) - (BitcoinAmount a2) = BitcoinAmount (fromIntegral . capTo21Mill $ a1-a2)
    abs = id    -- Always positive
    signum (BitcoinAmount 0) = BitcoinAmount 0
    signum (BitcoinAmount _) = BitcoinAmount 1
    fromInteger = BitcoinAmount . fromIntegral . capTo21Mill

instance Enum BitcoinAmount where
    toEnum = BitcoinAmount . fromIntegral . capTo21Mill . fromIntegral
    fromEnum (BitcoinAmount amount) = fromIntegral amount

instance Real BitcoinAmount where
    toRational (BitcoinAmount amount) = toRational amount

instance Integral BitcoinAmount where
    toInteger (BitcoinAmount int) = int
    quotRem (BitcoinAmount a1) (BitcoinAmount a2) =
        ( BitcoinAmount (fromIntegral . capTo21Mill $ res1)
        , BitcoinAmount (fromIntegral . capTo21Mill $ res2)
        )
            where (res1,res2) = quotRem a1 a2

instance Bounded BitcoinAmount where
    minBound = BitcoinAmount 0
    maxBound = BitcoinAmount $ round $ (21e6 :: Ratio Integer) * (1e8 :: Ratio Integer)

-- | Convert to 21 million, zero as floor
capTo21Mill :: Integer -> Word64
capTo21Mill i = fromIntegral $
    max 0 cappedValue
        where
            cappedValue = min i $ fromIntegral (maxBound :: BitcoinAmount)

instance Ser.Serialize BitcoinAmount where
    put = SerPut.putWord64le . fromIntegral . toInteger
    get = BitcoinAmount . fromIntegral <$> SerGet.getWord64le
