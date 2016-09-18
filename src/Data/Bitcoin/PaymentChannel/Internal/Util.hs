{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bitcoin.PaymentChannel.Internal.Util
(
    module Data.Bitcoin.PaymentChannel.Internal.Util
,   module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
,   module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime
,   cs
)
    where

import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime

import Data.String (fromString)

import qualified Data.Serialize as Bin
import qualified Data.Serialize.Get as BinGet

import           Data.Typeable
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Aeson.Types as JSON
import qualified Network.Haskoin.Crypto as HC
import           Data.String.Conversions (cs)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)


mapLeft f  = either (Left . f) Right
mapRight f = either Left (Right . f)

dummyHash256 = fromString "3d96c573baf8f782e5f5f33dc8ce3c5bae654cbc888e9a3bbb8185a75febfd76" :: HC.Hash256

toHexString :: B.ByteString -> String
toHexString =  C.unpack . B16.encode

toHexBS :: B.ByteString -> B.ByteString
toHexBS =  B16.encode

fromHexBS :: B.ByteString -> B.ByteString
fromHexBS = fst . B16.decode

fromHexString :: String -> B.ByteString
fromHexString hexStr =
    case (B16.decode . C.pack) hexStr of
        (bs,e) ->
            if B.length e /= 0 then B.empty else bs

serialize :: Bin.Serialize a => a -> B.ByteString
serialize = Bin.encode

deserEither :: forall a. (Typeable a, Bin.Serialize a) => B.ByteString -> Either String a
deserEither bs = do
    let eitherRes' = BinGet.runGetPartial (Bin.get :: BinGet.Get a) bs
    handleResult eitherRes'
    where
        handleResult eitherRes = case eitherRes of
            BinGet.Done val _           -> Right val
            BinGet.Partial feedFunc     -> handleResult $ feedFunc B.empty
            BinGet.Fail e leftoverBS    -> Left $
                "Type: " ++ show (typeOf (undefined :: a)) ++
                ". Error: " ++ e ++
                ". Data consumed (" ++ show offset ++ " bytes): " ++
                    toHexString (B.take offset bs) ++
                ". Unconsumed data: (" ++ show ( B.length bs - fromIntegral offset ) ++
                " bytes): " ++
                    toHexString leftoverBS
                        where offset = B.length bs - B.length leftoverBS

deserHex :: (Typeable a, Bin.Serialize a) => T.Text -> JSON.Parser a
deserHex = either
   (fail . ("failed to decode hex: " ++)) return .
   deserEither . fromHexBS . encodeUtf8

serHex :: Bin.Serialize a => a -> T.Text
serHex = decodeUtf8 . toHexBS . Bin.encode