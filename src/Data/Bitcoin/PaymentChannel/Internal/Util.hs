{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bitcoin.PaymentChannel.Internal.Util where

import Data.String (fromString)
import qualified Data.Serialize as Ser
import qualified Data.Serialize.Put as SerPut
import qualified Data.Serialize.Get as SerGet
import qualified Data.Binary as Bin
import qualified Data.Binary.Put as BinPut
import qualified Data.Binary.Get as BinGet
import Data.Binary.Put (putWord32le)
import Data.Binary.Get (getWord32le)
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
import           Data.Typeable
-- import           Data.Text.p

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

deserEither :: forall m a. (Typeable a, Bin.Binary a) => BL.ByteString -> Either String a
deserEither bs = do
    let eitherRes = Bin.decodeOrFail bs
    case eitherRes of
        Left (leftoverBS,offset,e)    -> Left $
            "Type: " ++ show (typeOf (undefined :: a)) ++
            ". Error: " ++ e ++
            ". Data consumed (" ++ show offset ++ " bytes): " ++
            toHexString (BL.toStrict $ BL.take offset bs) ++
            ". Unconsumed data: (" ++ show ( BL.length bs - fromIntegral offset ) ++ " bytes): " ++
            toHexString (BL.toStrict leftoverBS)
        Right (_,_,val) -> Right val


----------BITCOIN-----------

replaceScriptInput :: B.ByteString -> HT.Tx -> HT.Tx
replaceScriptInput scriptIn (HT.Tx v (txIn:_) txOut lt)  =
    HT.Tx v [newTxIn] txOut lt
        where newTxIn = txIn { HT.scriptInput = scriptIn }
replaceScriptInput _ (HT.Tx _ [] _ _) =
    error "cannot replace scriptInput without any inputs"

removeOutputs :: HT.Tx -> HT.Tx
removeOutputs tx = tx { HT.txOut = [] }

appendOutput :: HT.Tx -> HT.TxOut -> HT.Tx
appendOutput tx@HT.Tx{ HT.txOut = oldOuts } txOut =
    tx { HT.txOut = oldOuts ++ [txOut] }

-- |Data type representing a Bitcoin LockTime, which specifies a point in time.
--  Derive a 'BitcoinLockTime' from a 'Data.Time.Clock.UTCTime' using 'fromDate'.
data BitcoinLockTime =
    -- |A value of "n" represents the point in time at which Bitcoin block number "n" appears
    LockTimeBlockHeight Word32 |
    -- |Specifies a point in time using a timestamp with 1-second accuraccy
    LockTimeDate UTCTime deriving (Eq, Ord, Typeable)

instance Show BitcoinLockTime where
    show (LockTimeBlockHeight blockNum) = "block number " ++ show blockNum
    show (LockTimeDate date) = show date

-- | Convert from Bitcoin format ('Word32')
parseBitcoinLocktime :: Word32 -> BitcoinLockTime
parseBitcoinLocktime i
    | i <   500000000 = LockTimeBlockHeight i
    | i >=  500000000 = LockTimeDate $ posixSecondsToUTCTime (fromIntegral i)
    | otherwise       = error "GHC bug?"

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
