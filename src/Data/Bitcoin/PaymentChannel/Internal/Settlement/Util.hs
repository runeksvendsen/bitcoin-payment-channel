module Data.Bitcoin.PaymentChannel.Internal.Settlement.Util
(
  module Data.Bitcoin.PaymentChannel.Internal.Settlement.Util
, module Data.Bitcoin.PaymentChannel.Internal.Settlement.Types
) where

import Data.Bitcoin.PaymentChannel.Internal.Settlement.Types
import Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
import Data.Bitcoin.PaymentChannel.Internal.Class.Value     (HasValue(..))
-- import           Data.Time.Clock                            (getCurrentTime)


fromState :: ReceiverPaymentChannelI a -> SettleInfo
fromState rpc = SettleInfo (valueOf rpc) (pcsPaymentCount $ rpcState rpc)



