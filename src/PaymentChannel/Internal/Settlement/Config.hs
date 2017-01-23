module PaymentChannel.Internal.Settlement.Config where

import qualified Network.Haskoin.Script as HS

-- |Sign everything, and do not allow additional inputs to be added afterwards.
serverSigHash :: HS.SigHash
serverSigHash = HS.SigAll False
