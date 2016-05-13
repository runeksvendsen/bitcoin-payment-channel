module Data.Bitcoin.PaymentChannel.Util
(
getFundingAddress
)
where

import Data.Bitcoin.PaymentChannel.Internal.Script
    (getP2SHFundingAddress)

getFundingAddress = getP2SHFundingAddress