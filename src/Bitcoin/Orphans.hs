module Bitcoin.Orphans where

import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import qualified Network.Haskoin.Script         as HS
import qualified Data.Ord                       as Ord



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
