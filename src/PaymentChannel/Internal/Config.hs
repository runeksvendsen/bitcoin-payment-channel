{-# LANGUAGE DataKinds #-}
module PaymentChannel.Internal.Config
(
  module PaymentChannel.Internal.Config
, module Bitcoin.Config
)
where

import           Bitcoin.Config
import           Data.Word
import qualified Data.Tagged as     Tag


type Hour = Tag.Tagged "Hour" Word32

toSeconds :: Num a => Hour -> a
toSeconds = fromIntegral . (* 3600) . Tag.unTagged

configSettlePeriod :: Hour
configSettlePeriod = 12

getSettlePeriod = configSettlePeriod
getDustLimit = configDustLimit


