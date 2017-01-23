{-# LANGUAGE ScopedTypeVariables #-}

module PaymentChannel.Internal.Util
(
    module PaymentChannel.Internal.Util
-- ,   module Bitcoin.Util
-- ,   module Bitcoin.LockTime.Types
,   module Bin, module BinGet, module BinPut
,   module JSON, module JSONT, module JSONUtil
,   module Sci
,   module Data.Maybe
,   module Data.Either
,   B.ByteString
,   Generic
,   Typeable
,   Int64
,   module Tagged
,   cs
,   fmapL
,   (<>)
,   module Ctrl
)
where

import PaymentChannel.Internal.Serialization.JSON as JSONUtil


import Data.Serialize       as Bin
import Data.Serialize.Get   as BinGet
import Data.Serialize.Put   as BinPut
import Data.Aeson           as JSON hiding (Result(..), encode, decode)
import Data.Aeson.Types     as JSONT hiding (Result(..))
import Data.Scientific      as Sci

import           Data.Monoid                    ((<>))
import           Data.Maybe
import           Data.Either
import           Data.Tagged                    as Tagged hiding (witness)
import qualified Data.ByteString.Base16         as B16
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as C
import qualified Data.Text                      as T
import qualified Data.Aeson.Types               as JSON
import           Data.Int                   (Int64)
import           Data.String.Conversions    (cs)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.EitherR               (fmapL)
import           Control.Monad              as Ctrl (forM, mapM, (>=>), (<=<))
import           GHC.Generics               (Generic)
import           Data.Typeable              (Typeable, typeOf)
import qualified Network.Haskoin.Crypto     as HC

toInt :: (Integral a, Num b) => a -> b
toInt = fromIntegral


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


-- dummyPubkey :: HC.PubKeyC
-- dummyPubkey = mkDummyPubkey "0323dcd0a7481cb90e3583923701b14d0b6757ebd76bf5b49e696c61f193d7a489"

mkDummyPubkey :: B.ByteString -> HC.PubKeyC
mkDummyPubkey hexStr =
    either (error "mkDummyPubkey: Invalid dummy pubkey data") id (Bin.decode pkBS)
        where pkBS = fst $ B16.decode hexStr


mkDummySig :: B.ByteString -> HC.Signature
mkDummySig hexStr =
    fromMaybe (error "dummySig: Invalid dummy sig data") (HC.decodeDerSig sigBS)
        where sigBS = fst $ B16.decode hexStr

dummySig = mkDummySig "304402204202cdb61cb702aa62de312a8e5eada817d90c4e26c8b696780b14d1576f204f02203c134d0acb057d917508ca9baab241a4f66ebea32f7acceeaf621a334927e17701"
