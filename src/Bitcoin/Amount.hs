module Bitcoin.Amount
(
  BtcAmount
)

where

import           PaymentChannel.Internal.Util
import           Data.Word
import           Data.Ratio


-- |Represents a bitcoin amount as number of satoshis.
--  1 satoshi = 1e-8 bitcoin. 1e8 satohis = 1 bitcoin.
--  Only amounts >= 0 can be represented, and 'fromInteger' caps to a 'Word64'.
--  It is thus not possible to eg. construct a negative BtcAmount which, when added to
--  another BtcAmount, subtracts from its value. Adding two large amounts together will
--  never overflow, nor will subtraction underflow.
newtype BtcAmount = MkBitcoinAmount Integer
    deriving (Eq, Ord)
instance Show BtcAmount where
    show amount = show (toInteger amount) ++ " satoshi"

instance Num BtcAmount where
    (MkBitcoinAmount a1) * (MkBitcoinAmount a2) = mkCapped $ a1*a2
    (MkBitcoinAmount a1) + (MkBitcoinAmount a2) = mkCapped $ a1+a2
    (MkBitcoinAmount a1) - (MkBitcoinAmount a2) = mkCapped $ a1-a2
    abs = id    -- Always positive
    signum (MkBitcoinAmount 0) = MkBitcoinAmount 0
    signum (MkBitcoinAmount _) = MkBitcoinAmount 1
    fromInteger = mkCapped

instance Enum BtcAmount where
    toEnum = mkCapped . fromIntegral
    fromEnum (MkBitcoinAmount amount) = fromIntegral amount

instance Real BtcAmount where
    toRational (MkBitcoinAmount amount) = toRational amount

instance Integral BtcAmount where
    toInteger (MkBitcoinAmount int) = int
    quotRem (MkBitcoinAmount a1) (MkBitcoinAmount a2) =
        (mkCapped res1, mkCapped res2)
            where (res1,res2) = quotRem a1 a2

instance Bounded BtcAmount where
    minBound = MkBitcoinAmount 0
    maxBound = MkBitcoinAmount $ round $ (21e6 :: Ratio Integer) * (1e8 :: Ratio Integer)

mkCapped :: Integer -> BtcAmount
mkCapped = fromIntegral . capTo21Mill

-- | Convert to 21 million, zero as floor
capTo21Mill :: Integer -> Word64
capTo21Mill i = fromIntegral $
    max 0 cappedValue
        where
            cappedValue = min i $ fromIntegral (maxBound :: BtcAmount)

instance Serialize BtcAmount where
    put = putWord64le . fromIntegral . toInteger
    get = mkCapped . fromIntegral <$> getWord64le

instance ToJSON BtcAmount where
    toJSON amt = Number $ scientific
        (fromIntegral $ toInteger amt) 0

instance FromJSON BtcAmount where
    parseJSON = withScientific "BtcAmount" $
        fmap (\w -> mkCapped $ fromIntegral (w :: Word64)) . parseJSONWord


