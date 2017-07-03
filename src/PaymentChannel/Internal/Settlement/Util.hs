module PaymentChannel.Internal.Settlement.Util
(
  module PaymentChannel.Internal.Settlement.Util
, module PaymentChannel.Internal.Settlement.Types
) where

import qualified Data.List.NonEmpty                       as NE
import qualified Network.Haskoin.Crypto                   as HC
import qualified Network.Haskoin.Script                   as HS
import qualified PaymentChannel.Internal.ChanScript       as S
import           PaymentChannel.Internal.Class.Value      (HasValue (..))
import           PaymentChannel.Internal.Payment
import           PaymentChannel.Internal.Receiver.Types
import           PaymentChannel.Internal.Settlement.Types
import           PaymentChannel.Internal.Util




infoFromState :: ServerPayChanG kd sd -> SettleInfo
infoFromState rpc = error "STUB" -- SettleInfo (error "STUB") (mdPayCount $ rpcMetadata rpc)
