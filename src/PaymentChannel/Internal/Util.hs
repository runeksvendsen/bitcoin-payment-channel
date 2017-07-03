{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module PaymentChannel.Internal.Util
( module PaymentChannel.Internal.Util
, module Bitcoin.Internal.Util
, module JSONUtil
)
where

import           Bitcoin.Internal.Util
import           PaymentChannel.Internal.Serialization.JSON as JSONUtil

-- MonadPast
import           Control.Monad.Time
import           Data.Functor.Identity
import           Data.Time.Clock.POSIX

newtype MonadPast a = MonadPast (Identity a)
    deriving (Functor, Applicative, Monad)

instance MonadTime MonadPast where
    currentTime = MonadPast $ Identity $ posixSecondsToUTCTime $ realToFrac 0

-- | A 'MonadTime' result as if it were Jan 1 1970
resultFromThePast :: MonadPast a -> a
resultFromThePast (MonadPast (Identity a)) = a
-- MonadPast
