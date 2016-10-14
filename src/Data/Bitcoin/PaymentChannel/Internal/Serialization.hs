{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Bitcoin.PaymentChannel.Internal.Serialization where

import           Data.Bitcoin.PaymentChannel.Internal.Types
import           Data.Bitcoin.PaymentChannel.Internal.Util
import           Data.Bitcoin.PaymentChannel.Internal.Error
import qualified Network.Haskoin.Transaction as HT
import           Data.Aeson
import           Data.Aeson.Types (Parser, Pair)
import           Data.Scientific (Scientific, scientific, toBoundedInteger)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import qualified Data.Serialize     as Bin
import qualified Data.Serialize.Put as BinPut
import qualified Data.Serialize.Get as BinGet

import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Data.Word (Word64)
import           Data.EitherR (fmapL)
import           Data.Typeable
import qualified Data.Tagged as Tag

-- Generic PayChanError instance
instance Bin.Serialize PayChanError

--- JSON
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
    toJSON = object . toJSONObject

toJSONObject :: Payment -> [Pair]
toJSONObject (CPayment changeVal (CPaymentSignature sig flag)) =
    [   "change_value"      .= changeVal
    ,   "signature_data"    .= String (serHex sig)
    ,   "sighash_flag"      .= String (serHex flag)
    ]

parseJSONObject :: Object -> Parser Payment
parseJSONObject o = CPayment
       <$>      o .: "change_value"
       <*>     (CPaymentSignature <$>
                   (o .: "signature_data" >>= withText "SigDataHex" deserHex) <*>
                   (o .: "sighash_flag"   >>= withText "SigHashFlagHex" deserHex))

instance ToJSON FullPayment where
    toJSON CFullPayment {
        fpPayment       = payment,
        fpOutPoint      = (HT.OutPoint txid vout), fpRedeemScript = script,
        fpChangeAddr    = addr } =
            object $
                toJSONObject payment ++
                [   "funding_txid"     .= txid
                ,   "funding_vout"     .= vout
                ,   "redeem_script"    .= String (serHex script)
                ,   "change_address"   .= addr
                ]

instance FromJSON FullPayment where
    parseJSON = withObject "FullPayment" parseFullPayment

parseFullPayment :: Object -> Parser FullPayment
parseFullPayment o = CFullPayment
    <$>     parseJSONObject o
    <*>     (HT.OutPoint <$>
                 o .: "funding_txid" <*>
                 o .: "funding_vout")
    <*>     (o .: "redeem_script" >>= withText "RedeemScriptHex" deserHex)
    <*>      o .: "change_address"

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
instance Bin.Serialize ChanScript where
    put (ChanScript s) =
        BinPut.putWord16be scriptBSLen >>
        BinPut.putByteString scriptBS
            where scriptBS    = Bin.encode s
                  scriptBSLen = fromIntegral $ B.length scriptBS
    get = either error ChanScript . Bin.decode <$>
            (BinGet.getWord16be >>=
             BinGet.getByteString . fromIntegral)

deriving instance Bin.Serialize SendPubKey
deriving instance Bin.Serialize RecvPubKey

instance Bin.Serialize PaymentChannelState where
    put (CPaymentChannelState cfg par fti payConf payCount valLeft sig) =
        Bin.put cfg >> Bin.put par >> Bin.put fti >> Bin.put payConf >> Bin.put payCount >>
        Bin.put valLeft >> Bin.put sig
    get = CPaymentChannelState <$> Bin.get <*> Bin.get <*>
        Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get

instance Bin.Serialize ChannelParameters where
    put (CChannelParameters pks pkr lt) =
        Bin.put pks >> Bin.put pkr >> Bin.put lt
    get = CChannelParameters <$> Bin.get <*> Bin.get <*> Bin.get

instance Bin.Serialize FundingTxInfo where
    put (CFundingTxInfo h idx val) =
        Bin.put h >> BinPut.putWord32be idx >> Bin.put val
    get = CFundingTxInfo <$> Bin.get <*> BinGet.getWord32be <*> Bin.get

instance Bin.Serialize PaymentTxConfig where
    put (CPaymentTxConfig sendAddr) =
        Bin.put sendAddr
    get = CPaymentTxConfig <$> Bin.get

instance Bin.Serialize Config where
    put (Config dl sp) =
        Bin.put dl >> Bin.put (Tag.unTagged sp)
    get = Config <$> Bin.get <*> fmap Tag.Tagged Bin.get

instance Bin.Serialize Payment where
    put (CPayment val sig) =
        Bin.put val >> Bin.put sig
    get = CPayment <$> Bin.get <*> Bin.get

instance Bin.Serialize FullPayment where
    put (CFullPayment p op script addr) =
        Bin.put p >> Bin.put op >> Bin.put (ChanScript script) >> Bin.put addr
    get = CFullPayment <$> Bin.get <*> Bin.get <*> fmap getScript Bin.get <*> Bin.get

instance Bin.Serialize PaymentSignature where
    put (CPaymentSignature sig sigHash) =
        Bin.put sig >> Bin.put sigHash
    get = CPaymentSignature <$> Bin.get <*> Bin.get

--- Misc.
instance Show Payment where
    show (CPayment val sig) =
        "<Payment: valLeft=" ++ show val ++
        ", sig=" ++ toHexString (Bin.encode sig) ++ ">"

instance Show FullPayment where
    show (CFullPayment p op script addr) =
        "<FullPayment: payment = " ++ show p ++ " " ++
        show (op, script, addr) ++ ">"

-- Needed to convert from Scientific
instance Bounded BitcoinAmount where
    minBound = BitcoinAmount 0
    maxBound = BitcoinAmount $ round $ 21e6 * 1e8


--- Util
b64Encode :: Bin.Serialize a => a -> B.ByteString
b64Encode = B64.encode . Bin.encode

b64Decode :: (Typeable a, Bin.Serialize a) => B.ByteString -> Either String a
b64Decode b64 =
    concatErr "failed to deserialize parsed base64 data: " . deserEither =<<
    (concatErr "failed to parse base64 data: ") (b64Decode b64)
        where
            b64Decode = B64.decode . padToMod4
            concatErr e = fmapL (e ++)

txtB64Encode :: Bin.Serialize a => a -> T.Text
txtB64Encode = decodeUtf8 . b64Encode

txtB64Decode :: (Typeable a, Bin.Serialize a) => T.Text -> Parser a
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
        equalASCIIChar = 61
    in
        B.concat [bs, B.replicate numPadChars equalASCIIChar]
