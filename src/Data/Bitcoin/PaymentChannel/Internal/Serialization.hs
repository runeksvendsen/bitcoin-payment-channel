{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Bitcoin.PaymentChannel.Internal.Serialization where

import           Data.Bitcoin.PaymentChannel.Internal.Types
import           Data.Bitcoin.PaymentChannel.Internal.Util
                            (deserEither, toHexString,
                             parseBitcoinLocktime, BitcoinLockTime(..), toWord32)

import           Data.Aeson (Value(Number), FromJSON(..), ToJSON(..),
                              withText, withScientific)
import           Data.Aeson.Types (Parser)
import           Data.Scientific (Scientific, scientific, toBoundedInteger)
import           Data.Text.Encoding       (decodeLatin1, encodeUtf8)
import           Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.Binary as Bin
import qualified Data.Binary.Put as BinPut
import qualified Data.Binary.Get as BinGet
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Data.Word (Word64)
import           Data.EitherR (fmapL)
import           Data.Typeable



---- JSON
deriving instance ToJSON SendPubKey
deriving instance FromJSON SendPubKey
deriving instance ToJSON RecvPubKey
deriving instance FromJSON RecvPubKey

instance ToJSON BitcoinLockTime where
    toJSON blt = Number $ scientific
        (fromIntegral $ toWord32 blt) 0

instance FromJSON BitcoinLockTime where
    parseJSON = withScientific "BitcoinLockTime" $
        fmap (parseBitcoinLocktime . fromIntegral) . parseJSONInt

instance ToJSON Payment where
    toJSON = toJSON . txtB64Encode

instance FromJSON Payment where
    parseJSON = withText "Payment" txtB64Decode

instance ToJSON BitcoinAmount where
    toJSON amt = Number $ scientific
        (fromIntegral $ toInteger amt) 0

instance FromJSON BitcoinAmount where
    parseJSON = withScientific "BitcoinAmount" $
        fmap fromIntegral . parseJSONInt

instance ToJSON PaymentChannelState where
    toJSON = toJSON . txtB64Encode

instance FromJSON PaymentChannelState where
    parseJSON = withText "PaychanState" txtB64Decode


--- Binary
deriving instance Bin.Binary SendPubKey
deriving instance Bin.Binary RecvPubKey

instance Bin.Binary PaymentChannelState where
    put (CPaymentChannelState par fti payConf valLeft (Just sig)) =
        Bin.put par >> Bin.put fti >> Bin.put payConf >> Bin.put valLeft
        >> BinPut.putWord8 1 >> Bin.put sig
    put (CPaymentChannelState par fti payConf valLeft Nothing) =
            Bin.put par >> Bin.put fti >> Bin.put payConf >> Bin.put valLeft
            >> BinPut.putWord8 0
    get = CPaymentChannelState <$> Bin.get <*> Bin.get <*>
        Bin.get <*> Bin.get <*> (BinGet.getWord8 >>=
        \w -> case w of
            1 -> fmap Just Bin.get
            _ -> return Nothing)

instance Bin.Binary ChannelParameters where
    put (CChannelParameters pks pkr lt) =
        Bin.put pks >> Bin.put pkr >> Bin.put lt
    get = CChannelParameters <$> Bin.get <*> Bin.get <*> Bin.get

instance Bin.Binary FundingTxInfo where
    put (CFundingTxInfo h idx val) =
        Bin.put h >> BinPut.putWord32be idx >> Bin.put val
    get = CFundingTxInfo <$> Bin.get <*> BinGet.getWord32be <*> Bin.get

instance Bin.Binary PaymentTxConfig where
    put (CPaymentTxConfig sendAddr) =
        Bin.put sendAddr
    get = CPaymentTxConfig <$> Bin.get

instance Bin.Binary Payment where
    put (CPayment val sig) = Bin.put val >> Bin.put sig
    get = CPayment <$> Bin.get <*> Bin.get

instance Bin.Binary PaymentSignature where
    put ps = Bin.put (psSig ps)
        >> Bin.put (psSigHash ps)
    get = CPaymentSignature <$> Bin.get <*> Bin.get


--- Misc.
instance Show Payment where
    show (CPayment val sig) =
        "<Payment: valLeft=" ++ show val ++
        ", sig=" ++ toHexString (toStrict $ Bin.encode sig) ++ ">"

    -- Needed to convert from Scientific
instance Bounded BitcoinAmount where
    minBound = BitcoinAmount 0
    maxBound = BitcoinAmount $ round $ 21e6 * 1e8


--- Util
b64Encode :: Bin.Binary a => a -> B.ByteString
b64Encode = B64.encode . toStrict . Bin.encode

b64Decode :: (Typeable a, Bin.Binary a) => B.ByteString -> Either String a
b64Decode b64 =
    concatErr "failed to deserialize parsed base64 data: " . deserEither . BL.fromStrict =<<
    (concatErr "failed to parse base64 data: ") (b64Decode b64)
        where
            b64Decode = B64.decode . padToMod4
            concatErr e = fmapL (e ++)

txtB64Encode :: Bin.Binary a => a -> T.Text
txtB64Encode = decodeLatin1 . b64Encode

txtB64Decode :: (Typeable a, Bin.Binary a) => T.Text -> Parser a
txtB64Decode = either fail return . b64Decode . encodeUtf8

parseJSONInt :: Scientific -> Parser Integer
parseJSONInt s =
    case toBoundedInteger s of
        Just (BitcoinAmount i) -> return i
        Nothing -> fail $ "failed to decode JSON number to integer. data: " ++ show s

parseJSONWord :: Scientific -> Parser Word64
parseJSONWord s =
    case toBoundedInteger s of
        Just w -> return w
        Nothing -> fail $ "failed to decode JSON number to Word64. data: " ++ show s

padToMod4 :: B.ByteString -> B.ByteString
padToMod4 bs =
    let
        lastGroupSize = B.length bs `mod` 4
        numPadChars = if lastGroupSize > 0 then 4 - lastGroupSize else 0
    in
        B.concat [bs, B.replicate numPadChars 61] -- 61: '=' ASCII
