module Data.Bitcoin.PaymentChannel.Internal.Receiver.Settle where

import Data.Bitcoin.PaymentChannel.Internal.Receiver.Types
import qualified Data.Bitcoin.PaymentChannel.Internal.Settlement.Util as Settle
import Data.Bitcoin.PaymentChannel.Internal.Error
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.Metadata.Util
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT


newMetaAfterSettle :: RecvPayChanI a -> MetadataI a
newMetaAfterSettle rpc@CReceiverPaymentChannel{rpcMetadata = prevMd} =
    prevMd
        { mdSettledValue   = Settle.fromState rpc : mdSettledValue prevMd :: [Settle.SettleInfo]
        , mdUnsettledValue = 0
        , mdChannelStatus  = statusAfterSettlement rpc
        }

-- |What would the status of the payment channel be
--   if the settlement transaction were published right now?
statusAfterSettlement :: ReceiverPaymentChannelI a -> PayChanStatus
statusAfterSettlement rpc =
    if changeAddress rpc == fundingAddress rpc then
        ReadyForPayment
    else
        ChanClosed
  where
    changeAddress = pcsClientChangeAddress . rpcState
    fundingAddress = getP2SHFundingAddress . pcsParameters . rpcState

settleChanState :: HT.OutPoint -> RecvPayChanI a -> RecvPayChanI a
settleChanState (HT.OutPoint h i) rpc@CReceiverPaymentChannel{rpcState = pcs} =
    undefined
        where
            newFti = CFundingTxInfo h i (pcsClientChangeVal pcs)
            newPCS = pcs { pcsFundingTxInfo = newFti, pcsPaymentCount = 0 }


