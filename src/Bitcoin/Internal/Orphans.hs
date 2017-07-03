module Bitcoin.Internal.Orphans where

import           Data.Aeson
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as B16
import qualified Data.Ord                as Ord
import           Data.String.Conversions (cs)
import qualified Network.Haskoin.Crypto  as HC
import qualified Network.Haskoin.Script  as HS



-- | SigSingle first, then SigNone, then SigAll
instance Ord.Ord HS.SigHash where
    compare s1 s2 =
            case (s1,s2) of
                (HS.SigSingle  _,               _) -> Ord.LT
                (HS.SigNone    _, HS.SigSingle  _) -> Ord.GT
                (HS.SigNone    _, HS.SigNone    _) -> Ord.EQ
                (HS.SigNone    _, HS.SigAll     _) -> Ord.LT
                (HS.SigAll     _,               _) -> Ord.GT
                (HS.SigUnknown{},               _) -> Ord.GT
                (              _, HS.SigUnknown{}) -> Ord.GT


instance FromJSON HC.Hash256 where
    parseJSON =
        withText "Hash256" $ \src -> do
        bs <- decodeHex . BS.take 64 . cs $ src
        let parseErr = "parse fail. input data: " ++ show src
        maybe (fail parseErr) return (HC.bsToHash256 bs)
      where
        decodeHex src = let (res,leftovers) = B16.decode src in
            if leftovers == BS.empty
                then return res
                else fail $ "failed to decode from: " ++ show src

instance ToJSON HC.Hash256 where
    toJSON = String . cs . B16.encode . HC.getHash256
