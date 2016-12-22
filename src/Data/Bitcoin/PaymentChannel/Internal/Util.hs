{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bitcoin.PaymentChannel.Internal.Util
(
    module Data.Bitcoin.PaymentChannel.Internal.Util
-- ,   module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
-- ,   module Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime
,   module Bin, module BinGet, module BinPut
,   module JSON, module JSONT, module JSONUtil
,   module Sci
,   Generic
,   Typeable
,   cs
,   fmapL
,   module Ctrl
)
    where

-- import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
-- import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.LockTime
import Data.Bitcoin.PaymentChannel.Internal.Serialization.JSON as JSONUtil


import Data.Serialize       as Bin
import Data.Serialize.Get   as BinGet
import Data.Serialize.Put   as BinPut
import Data.Aeson           as JSON hiding (Result(..), encode, decode)
import Data.Aeson.Types     as JSONT hiding (Result(..), encode, decode)
import Data.Scientific      as Sci

import           Data.Typeable
import qualified Data.ByteString.Base16         as B16
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as C
import qualified Data.Text                      as T
import qualified Data.Aeson.Types               as JSON
import qualified Network.Haskoin.Crypto         as HC
import qualified Network.Haskoin.Transaction    as HT
import           Data.String.Conversions    (cs)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.EitherR               (fmapL)
import           Control.Monad              as Ctrl (forM, mapM, (>=>), (<=<))
import           GHC.Generics               (Generic)
import           Data.Typeable              (Typeable)

getIndexSafe :: [a] -> Int -> Maybe a
getIndexSafe l i = if i < 0 || i+1 > length l then Nothing else Just $ l !! i

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