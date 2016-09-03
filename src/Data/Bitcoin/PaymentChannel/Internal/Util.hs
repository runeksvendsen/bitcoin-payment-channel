{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bitcoin.PaymentChannel.Internal.Util
(
    module Data.Bitcoin.PaymentChannel.Internal.Util
,   module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
,   module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime
)
    where

import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime

import Data.String (fromString)
import qualified Data.Serialize as Ser
import qualified Data.Binary as Bin
import           Data.Typeable
import qualified  Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Network.Haskoin.Transaction as HT
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

serialize' :: Ser.Serialize a => a -> B.ByteString
serialize' = Ser.encode

serialize :: Bin.Binary a => a -> B.ByteString
serialize = BL.toStrict . Bin.encode

deserialize :: Bin.Binary a => B.ByteString -> a
deserialize = Bin.decode . BL.fromStrict

deserEither :: forall a. (Typeable a, Bin.Binary a) => BL.ByteString -> Either String a
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

