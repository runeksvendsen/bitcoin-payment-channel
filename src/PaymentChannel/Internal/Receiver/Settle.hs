module PaymentChannel.Internal.Receiver.Settle where

import           Bitcoin.Types
import qualified Network.Haskoin.Crypto                  as HC
import qualified Network.Haskoin.Transaction             as HT
import           PaymentChannel.Internal.Error
import           PaymentChannel.Internal.Metadata.Util
import           PaymentChannel.Internal.Payment
import           PaymentChannel.Internal.Receiver.Types
import qualified PaymentChannel.Internal.Settlement.Util as Settle
import           PaymentChannel.Types


{-
newMetaAfterSettle :: ServerPayChanI kd -> MetadataI a
newMetaAfterSettle rpc@MkServerPayChan{rpcMetadata = prevMd} =
    prevMd
        { mdSettledValue   = Settle.infoFromState rpc : mdSettledValue prevMd :: [Settle.SettleInfo]
        , mdUnsettledValue = 0
        , mdChannelStatus  = statusAfterSettlement rpc
        }

-- |What would the status of the payment channel be
--   if the settlement transaction were published right now?
statusAfterSettlement :: ServerPayChanI kd -> PayChanStatus
statusAfterSettlement rpc@MkServerPayChan{rpcState = pcs} =
    if changeAddress pcs /= fundingAddress rpc then     -- channelIsExhausted
        ChannelClosed
    else
        ReadyForPayment
  where
    changeAddress = clientChangeAddr . pcsPayment

-}

-- settleChanState :: SignedBtcTx ChanParams -> ServerPayChanI kd -> ServerPayChanI kd
-- settleChanState tx rpc@MkServerPayChan{rpcState = pcs} =
--     undefined
--         where
--             clientOutM = listToMaybe $ filter ()
--             newFti = CFundingTxInfo h i (pcsClientChangeVal pcs)
--             newPCS = pcs { pcsFundingTxInfo = newFti, pcsPaymentCount = 0 }


