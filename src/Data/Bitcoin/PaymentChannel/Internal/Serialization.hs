module Data.Bitcoin.PaymentChannel.Internal.Serialization where

import Data.Bitcoin.PaymentChannel.Internal.Types

import            Data.Aeson (FromJSON(..), ToJSON(..), withText) -- , parseJSON, toJSON)
import            Data.Text.Encoding       (decodeLatin1, encodeUtf8)
import            Data.ByteString.Lazy (toStrict, fromStrict)
import qualified  Data.Binary as Bin
import qualified  Data.Binary.Put as BinPut
import qualified  Data.Binary.Get as BinGet
import qualified  Data.ByteString.Base64 as B64


-------JSON---------
instance ToJSON Payment where
    toJSON = toJSON . decodeLatin1 . B64.encode . toStrict . Bin.encode

instance FromJSON Payment where
    parseJSON = withText "Payment" $
        pure . Bin.decode . fromStrict . B64.decodeLenient . encodeUtf8


------BINARY--------
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
    put (CPaymentTxConfig sendScript recvScript) =
        Bin.put sendScript >> Bin.put recvScript
    get = CPaymentTxConfig <$> Bin.get <*> Bin.get

instance Bin.Binary Payment where
    put (CPayment val sig) = Bin.put val >> Bin.put sig
    get = CPayment <$> Bin.get <*> Bin.get

instance Bin.Binary PaymentSignature where
    put ps = Bin.put (psSig ps)
        >> Bin.put (psSigHash ps)
    get = CPaymentSignature <$> Bin.get <*> Bin.get

