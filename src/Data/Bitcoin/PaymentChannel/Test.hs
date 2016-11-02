{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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
    { sendChan          :: SenderPaymentChannel
    , recvChan          :: ReceiverPaymentChannel
    , initPayAmount     :: BitcoinAmount
    , initRecvAmount    :: BitcoinAmount
    , initPayment       :: FullPayment
    , recvSignFunc      :: HC.Hash256 -> HC.Signature
    }

data ChannelPairResult = ChannelPairResult
    { resInitPair   :: ArbChannelPair
    , resSendChan   :: SenderPaymentChannel
    , resRecvChan   :: ReceiverPaymentChannel
    , resSendAmount :: [BitcoinAmount]
    , resRecvAmount :: [BitcoinAmount]
    , resPayList    :: [FullPayment]
    }

toInitResult :: ArbChannelPair -> ChannelPairResult
toInitResult initPair@(ArbChannelPair spc rpc amt rcv pay _) =
    ChannelPairResult initPair spc rpc [amt] [rcv] [pay]

instance Show ArbChannelPair where
    show (ArbChannelPair spc rpc _ _ _ _) =
        "SendState: " ++ show spc ++ "\n" ++
        "RecvState: " ++ show rpc

-- |Fold a payment of specified value into a 'ChannelPairResult'
doPayment :: ChannelPairResult -> BitcoinAmount -> ChannelPairResult
doPayment (ChannelPairResult initPair spc rpc sendList recvList payLst) amount =
    let
        (amountSent, pmn, newSpc) = sendPayment spc amount
        eitherRpc = recvPayment nowishTimestamp rpc pmn
    in
        case eitherRpc of
            Left e -> error (show e)
            Right (recvAmount, newRpc) ->
                ChannelPairResult initPair newSpc newRpc
                    (amountSent : sendList)
                    (recvAmount : recvList)
                    (pmn : payLst)

runChanPair :: ArbChannelPair -> [BitcoinAmount] -> (ChannelPairResult, [BitcoinAmount])
runChanPair chanPair paymentAmountList =
    (foldl doPayment (toInitResult chanPair) paymentAmountList, paymentAmountList)

instance Arbitrary ArbChannelPair where
    arbitrary = fmap fst mkChanPair

instance Arbitrary ChannelPairResult where
    arbitrary =
        fst <$> (runChanPair <$> arbitrary <*> arbitrary)

instance Arbitrary SenderPaymentChannel where
    arbitrary = fmap (sendChan . fst) mkChanPair

instance Arbitrary ReceiverPaymentChannel where
    arbitrary = fmap (recvChan . fst) mkChanPair

instance Arbitrary PaymentChannelState where
    arbitrary = fmap rpcState (arbitrary :: Gen ReceiverPaymentChannel)

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

newtype NonZeroBitcoinAmount = NonZeroBitcoinAmount { getAmount :: BitcoinAmount }

instance Arbitrary NonZeroBitcoinAmount where
    arbitrary = (NonZeroBitcoinAmount . fromIntegral) <$>
        choose (1, round $ 21e6 * 1e8 :: Integer)


mkChanParams :: Gen (ChannelParameters, (HC.PrvKeyC, HC.PrvKeyC))
mkChanParams = do
    -- sender key pair
    ArbitraryPubKeyC sendPriv sendPK <- arbitrary
    -- receiver key pair
    ArbitraryPubKeyC recvPriv recvPK <- arbitrary
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
        let (initPayActualAmount,initPayment,sendChan) = channelWithInitialPaymentOf defaultConfig
                cp fti (flip HC.signMsg sendPriv) (getFundingAddress cp) initPayAmount
        let eitherRecvChan = channelFromInitialPayment
                nowishTimestamp defaultConfig cp fti initPayment
        case eitherRecvChan of
            Left e -> error (show e)
            Right (initRecvAmount,recvChan) -> return
                    (ArbChannelPair
                        sendChan recvChan initPayActualAmount initRecvAmount initPayment
                        (flip HC.signMsg recvPriv),
                    initPayment)
