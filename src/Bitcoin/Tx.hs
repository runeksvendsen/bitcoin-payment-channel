module Bitcoin.Tx where

import Bitcoin.Types
import PaymentChannel.Internal.Util
import Bitcoin.LockTime.Util
import qualified Data.List.NonEmpty     as NE
import Control.Monad.Time


allInputsLocked :: (MonadTime m, HasLockTimeDate inType) => BtcTx inType a -> m Bool
allInputsLocked BtcTx{..} = do
    isLockedLst <- mapM (isLocked . btcInType) (NE.toList btcIns)
    return $ all (== True) isLockedLst




