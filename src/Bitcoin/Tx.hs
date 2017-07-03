module Bitcoin.Tx where

import Bitcoin.Types
import Bitcoin.LockTime.Util
import qualified Data.List.NonEmpty     as NE
import Control.Monad.Time


allInputsLocked ::
    ( MonadTime m
    , HasLockTimeDate r
    )
    => Seconds          -- ^ Lock expires this many seconds before actual expiration date
    -> BtcTx inType r a
    -> m Bool
allInputsLocked settlePeriodSeconds BtcTx{..} = do
    isLockedLst <- mapM (isLocked settlePeriodSeconds . btcCondScr) (NE.toList btcIns)
    return $ all (== True) isLockedLst
