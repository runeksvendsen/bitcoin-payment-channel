{-# LANGUAGE RecordWildCards #-}
module Data.Bitcoin.PaymentChannel.Internal.Metadata.Util
(
  module Data.Bitcoin.PaymentChannel.Internal.Metadata.Util
, module Data.Bitcoin.PaymentChannel.Internal.Metadata.Types
, module Data.Bitcoin.PaymentChannel.Internal.Types
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.Metadata.Types
import Data.Bitcoin.PaymentChannel.Internal.Settlement.Util
-- import qualified Data.Bitcoin.PaymentChannel.Internal.State as S


metaSetStatus :: PayChanStatus -> MetadataI a -> MetadataI a
metaSetStatus s md = md { mdChannelStatus = s }

metaGetStatus :: MetadataI a -> PayChanStatus
metaGetStatus Metadata{ mdChannelStatus = s } = s

metaTotalValXfer :: MetadataI a -> BitcoinAmount
metaTotalValXfer Metadata{..} =
    sum (map siValue mdSettledValue) + mdUnsettledValue
