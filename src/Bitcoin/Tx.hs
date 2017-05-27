module Bitcoin.Tx where

import Bitcoin.Types
import Bitcoin.LockTime.Util
import qualified Data.List.NonEmpty     as NE
import Control.Monad.Time


allInputsLocked ::
    ( MonadTime m
    , HasLockTimeDate inType
    )
    => Seconds          -- ^ Lock expires this many seconds before actual expiration date
    -> BtcTx inType a
    -> m Bool
allInputsLocked settlePeriodSeconds BtcTx{..} = do
    isLockedLst <- mapM (isLocked settlePeriodSeconds . btcInType) (NE.toList btcIns)
    return $ all (== True) isLockedLst
