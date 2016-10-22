module Data.Bitcoin.PaymentChannel.Test
(
    module Data.Bitcoin.PaymentChannel.Test
  , module Data.Bitcoin.PaymentChannel
  , module Data.Bitcoin.PaymentChannel.Types
  , module Data.Bitcoin.PaymentChannel.Internal.Types
  , module Data.Bitcoin.PaymentChannel.Internal.State
)

where

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types
import           Data.Bitcoin.PaymentChannel.Internal.Types
import           Data.Bitcoin.PaymentChannel.Util
import           Data.Bitcoin.PaymentChannel.Internal.State hiding (channelIsExhausted, channelValueLeft)

import qualified Network.Haskoin.Crypto as HC
import           Network.Haskoin.Test
import           Data.Time.Clock                 (UTCTime(..))
import           Data.Time.Calendar              (Day(..))

import Test.QuickCheck


-- TODO: We don't bother testing expiration time for now
nowishTimestamp :: UTCTime
nowishTimestamp = UTCTime (ModifiedJulianDay 57683) 0     -- 2016-10-22

mIN_CHANNEL_SIZE :: BitcoinAmount
mIN_CHANNEL_SIZE = cDustLimit defaultConfig * 2


data ArbChannelPair = ArbChannelPair
    SenderPaymentChannel ReceiverPaymentChannel [BitcoinAmount] [BitcoinAmount] (HC.Hash256 -> HC.Signature)

instance Show ArbChannelPair where
    show (ArbChannelPair spc rpc _ _ _) =
        "SendState: " ++ show spc ++ "\n" ++
        "RecvState: " ++ show rpc

doPayment :: ArbChannelPair -> BitcoinAmount -> ArbChannelPair
doPayment (ArbChannelPair spc rpc sendList recvList f) amount =
    let
        (amountSent, pmn, newSpc) = sendPayment spc amount
        eitherRpc = recvPayment nowishTimestamp rpc pmn
    in
        case eitherRpc of
            Left e -> error (show e)
            Right (recvAmount, newRpc) ->
                ArbChannelPair newSpc newRpc
                    (amountSent : sendList)
                    (recvAmount : recvList)
                    f

instance Arbitrary ArbChannelPair where
    arbitrary = fmap fst mkChanPair

instance Arbitrary PaymentChannelState where
    arbitrary = fmap getPCS mkChanPair
        where getPCS (ArbChannelPair _ rpc _ _ _ , _) = rpcState rpc

instance Arbitrary ChanScript where
    arbitrary = ChanScript . getRedeemScript <$> arbitrary

instance Arbitrary FullPayment where
    arbitrary = fmap snd mkChanPair

instance Arbitrary ChannelParameters where
    arbitrary = fmap fst mkChanParams

instance Arbitrary FundingTxInfo where
    arbitrary = do
        ArbitraryTxHash h <- arbitrary
        i <- arbitrary
        amt <- fmap fromIntegral
            (choose (fromIntegral mIN_CHANNEL_SIZE, round $ 21e6 * 1e8 :: Integer))
            :: Gen BitcoinAmount
        return $ CFundingTxInfo h i amt

instance Arbitrary BitcoinAmount where
    arbitrary = fromIntegral <$> choose (0, round $ 21e6 * 1e8 :: Integer)


mkChanParams :: Gen (ChannelParameters, (HC.PrvKey, HC.PrvKey))
mkChanParams = do
    -- sender key pair
    ArbitraryPubKey sendPriv sendPK <- arbitrary
    -- receiver key pair
    ArbitraryPubKey recvPriv recvPK <- arbitrary
    -- TODO: We use an expiration date far off into the future for now
    let lockTime = parseBitcoinLocktime 2524651200
    return (CChannelParameters
                (MkSendPubKey sendPK) (MkRecvPubKey recvPK) lockTime,
           (sendPriv, recvPriv))

mkChanPair :: Gen (ArbChannelPair, FullPayment)
mkChanPair = do
        (cp, (sendPriv, recvPriv)) <- mkChanParams
        fti <- arbitrary
        -- value of first payment
        initPayAmount <- arbitrary
        -- create states
        let (initPayActualAmount,paymnt,sendChan) = channelWithInitialPaymentOf defaultConfig
                cp fti (flip HC.signMsg sendPriv) (getFundingAddress cp) initPayAmount
        let eitherRecvChan = channelFromInitialPayment nowishTimestamp defaultConfig cp fti paymnt
        case eitherRecvChan of
            Left e -> error (show e)
            Right (initRecvAmount,recvChan) -> return
                    (ArbChannelPair
                        sendChan recvChan [initPayActualAmount] [initRecvAmount]
                        (flip HC.signMsg recvPriv),
                    paymnt)
