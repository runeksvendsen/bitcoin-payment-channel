module PaymentChannel.Internal.Settlement.Util
(
  module PaymentChannel.Internal.Settlement.Util
, module PaymentChannel.Internal.Settlement.Types
) where

import PaymentChannel.Internal.Util
import PaymentChannel.Internal.Payment
import qualified PaymentChannel.Internal.ChanScript as S
import PaymentChannel.Internal.Settlement.Types
import PaymentChannel.Internal.Receiver.Types
import PaymentChannel.Internal.Class.Value     (HasValue(..))
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import qualified Data.List.NonEmpty     as NE




infoFromState :: ServerPayChanI a -> SettleInfo
infoFromState rpc = error "STUB" -- SettleInfo (error "STUB") (mdPayCount $ rpcMetadata rpc)
