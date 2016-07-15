{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bitcoin.PaymentChannel.Internal.Util where

import Data.String (fromString)
import qualified Data.Serialize as Ser (Serialize, encode, get, put)
import qualified Data.Serialize.Put as SerPut (putWord64be)
import qualified Data.Serialize.Get as SerGet (getWord64be)
import qualified Data.Binary as Bin
import qualified Data.Binary.Put as BinPut
import qualified Data.Binary.Get as BinGet
import Data.Binary.Put (putWord32le)
import Data.Binary.Get (getWord32le, runGetOrFail)
import Data.Word
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified  Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC

mapLeft f  = either (Left . f) Right
mapRight f = either Left (Right . f)

dummyHash256 = fromString "3d96c573baf8f782e5f5f33dc8ce3c5bae654cbc888e9a3bbb8185a75febfd76" :: HC.Hash256

testHey = toHexString $ serialize $ HT.TxHash dummyHash256

toHexString :: B.ByteString -> String
toHexString =  C.unpack . B16.encode

toHexBS :: B.ByteString -> B.ByteString
toHexBS =  B16.encode

fromHexString :: String -> B.ByteString
fromHexString hexStr =
    case (B16.decode . C.pack) hexStr of
        (bs,e) ->
            if B.length e /= 0 then B.empty else bs


-- |Represents a bitcoin amount as number of satoshis.
--  1 satoshi = 1e-8 bitcoins.
--  Integer operations will never over- or underflow with this type.
--  Convert to a Word64 using 'toWord64', which caps the final amount.
newtype BitcoinAmount = CMoneyAmount Integer
    deriving (Eq, Ord, Num, Enum, Real, Integral)
instance Show BitcoinAmount where
    show ma = show (fromIntegral $ toWord64 ma) ++ " satoshi"

-- | Convert to 'Word64', with zero as floor, UINT64_MAX as ceiling
toWord64 :: BitcoinAmount -> Word64
toWord64 (CMoneyAmount i) = fromIntegral $
    max 0 cappedValue
        where
            cappedValue = min i $ fromIntegral (maxBound :: Word64)

instance Bin.Binary BitcoinAmount where
    put = BinPut.putWord64le . toWord64
    get = CMoneyAmount . fromIntegral <$> BinGet.getWord64le

instance Ser.Serialize BitcoinAmount where
    put = SerPut.putWord64be . toWord64
    get = CMoneyAmount . fromIntegral <$> SerGet.getWord64be


-- | Converts a pay-to-pubkey-hash address string to Script.
-- | Eg. \"1PDGytNA7EJ5XrJdTGKv11VAUFxKnsfwke\" into
-- | \"Script [OP_DUP, OP_HASH160, OP_PUSHDATA f3a5194fbf4b3556838e0f773b613a876737a2c6,
-- | OP_EQUALVERIFY, OP_CHECKSIG]\"
p2PKAddressToScript :: String -> Maybe HS.Script
p2PKAddressToScript addrStr =
    HS.encodeOutput . HS.PayPKHash <$> HC.base58ToAddr (C.pack addrStr)

-- | Converts a pay-to-script-hash address string to Script.
-- | Eg. \"2PDGytNA7EJ5XrJdTGKv11VAUFxKnsfwke\"
p2SHAddressToScript :: String -> Maybe HS.Script
p2SHAddressToScript addrStr =
    HS.encodeOutput . HS.PayScriptHash <$> HC.base58ToAddr (C.pack addrStr)

addressToScript :: HC.Address -> HS.ScriptOutput
addressToScript addr =
    case addr of
        a@(HC.PubKeyAddress _) -> HS.PayPKHash a
        a@(HC.ScriptAddress _) -> HS.PayScriptHash a

addressToScriptPubKeyBS :: HC.Address -> B.ByteString
addressToScriptPubKeyBS = HS.encodeOutputBS . addressToScript

serialize' :: Ser.Serialize a => a -> B.ByteString
serialize' = Ser.encode

serialize :: Bin.Binary a => a -> B.ByteString
serialize = BL.toStrict . Bin.encode

deserialize :: Bin.Binary a => B.ByteString -> a
deserialize = Bin.decode . BL.fromStrict

deserEither :: Bin.Binary a => B.ByteString -> Either String a
deserEither bs =
    case runGetOrFail Bin.get (BL.fromStrict bs) of
        Left (_,_,e) -> Left e
        Right (_,_,res) -> Right res

----------BITCOIN-----------

replaceScriptInput :: B.ByteString -> HT.Tx -> HT.Tx
replaceScriptInput scriptIn (HT.Tx v (txIn:_) txOut lt)  =
    HT.Tx v [newTxIn] txOut lt
        where newTxIn = txIn { HT.scriptInput = scriptIn }
replaceScriptInput scriptIn (HT.Tx v [] txOut lt) =
    error "cannot replace scriptInput without any inputs"

removeOutputs :: HT.Tx -> HT.Tx
removeOutputs tx = tx { HT.txOut = [] }

appendOutput :: HT.Tx -> HT.TxOut -> HT.Tx
appendOutput tx@HT.Tx{ HT.txOut = oldOuts } txOut =
    tx { HT.txOut = oldOuts ++ [txOut] }

-- |Data type representing a Bitcoin LockTime, which specifies a point in time.
--  Derive a 'BitcoinLockTime' from a 'Data.Time.Clock.UTCTime' using 'fromDate'.
data BitcoinLockTime =
    LockTimeBlockHeight Word32 |
    LockTimeDate UTCTime deriving (Eq, Ord)

instance Show BitcoinLockTime where
    show (LockTimeBlockHeight blockNum) = "block number " ++ show blockNum
    show (LockTimeDate date) = show date

-- | Convert from Bitcoin format ('Word32')
parseBitcoinLocktime :: Word32 -> BitcoinLockTime
parseBitcoinLocktime i
    | i <   500000000 = LockTimeBlockHeight i
    | i >=  500000000 = LockTimeDate $ posixSecondsToUTCTime (fromIntegral i)

-- | Convert to Bitcoin format ('Word32')
toWord32 :: BitcoinLockTime -> Word32
toWord32 (LockTimeBlockHeight i) = i
toWord32 (LockTimeDate date) =
    fromIntegral . round . utcTimeToPOSIXSeconds $ date

-- | Convert a 'Data.Time.Clock.UTCTime' to a 'BitcoinLockTime'
fromDate :: UTCTime -> BitcoinLockTime
fromDate = LockTimeDate

usesBlockHeight :: BitcoinLockTime -> Bool
usesBlockHeight (LockTimeBlockHeight _) = True
usesBlockHeight _ = False


instance Bin.Binary BitcoinLockTime where
    put = putWord32le . toWord32
    get = parseBitcoinLocktime <$> getWord32le


bitcoinPayPK :: HC.PubKey -> HS.Script
bitcoinPayPK pk = HS.encodeOutput $ HS.PayPKHash $ HC.pubKeyAddr pk
bitcoinPayPKBS = serialize . bitcoinPayPK
