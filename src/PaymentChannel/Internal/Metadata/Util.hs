{-# LANGUAGE RecordWildCards #-}
module PaymentChannel.Internal.Metadata.Util
(
  module PaymentChannel.Internal.Metadata.Util
, module PaymentChannel.Internal.Metadata.Types
, module PaymentChannel.Internal.Types
)
where

import           PaymentChannel.Internal.Metadata.Types
import           PaymentChannel.Internal.Settlement.Util
import           PaymentChannel.Internal.Types
-- import qualified PaymentChannel.Internal.State as S


metaSetStatus :: PayChanStatus -> MetadataI a -> MetadataI a
metaSetStatus s md = md { mdChannelStatus = s }

metaGetStatus :: MetadataI a -> PayChanStatus
metaGetStatus Metadata{ mdChannelStatus = s } = s

metaTotalValXfer :: MetadataI a -> BtcAmount
metaTotalValXfer Metadata{..} =
    sum (map siValue mdSettledValue) + mdUnsettledValue
