{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PaymentChannel.Internal.Types.MonadConf where

import           Bitcoin.Types
import           Control.Monad.Reader
import           Control.Monad.Trans.Reader     (Reader, runReader)
import           PaymentChannel.Internal.Config

newtype MonadConf a = MonadConf (Reader ServerSettings a)
    deriving (Functor, Applicative, Monad, MonadReader ServerSettings)

runConfM :: ServerSettings -> MonadConf a -> a
runConfM cfg (MonadConf r) = runReader r cfg

instance HasConfDustLimit MonadConf where
    confDustLimit = asks serverConfDustLimit

instance HasConfSettlePeriod MonadConf where
    confSettlePeriod = asks serverConfSettlePeriod
