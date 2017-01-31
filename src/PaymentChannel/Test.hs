{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
module PaymentChannel.Test
(
    module PaymentChannel.Test
  , module X
)

where

import PaymentChannel           as X
import PaymentChannel.Types     as X
import PaymentChannel.Util      as X
import PaymentChannel.Internal.Receiver.Types as X
import PaymentChannel.Internal.ChanScript as X

import qualified Network.Haskoin.Crypto as HC
import           Network.Haskoin.Test
import           Data.Time.Clock                 (UTCTime(..))
import           Data.Time.Calendar              (Day(..))
import           Control.Monad                   (foldM)

import Test.QuickCheck


maxCoins :: Integer
maxCoins = round $ (21e6 :: Double) * 1e8

mIN_CHANNEL_SIZE :: BtcAmount
mIN_CHANNEL_SIZE = configDustLimit * 2


data ArbChannelPair = ArbChannelPair
    { sendChan          :: ClientPayChan
    , recvChan          :: ServerPayChan
    , initPayAmount     :: BtcAmount
    , initRecvAmount    :: BtcAmount
    , initPayment       :: SignedPayment
    , recvPrvKey        :: HC.PrvKeyC
    } deriving (Generic, NFData)

data ChannelPairResult = ChannelPairResult
    { resInitPair       :: ArbChannelPair
    , resSendChan       :: ClientPayChan
    , resRecvChan       :: ServerPayChan
    , resSentAmounts    :: [BtcAmount]
    , resRecvdAmounts   :: [BtcAmount]
    , resPayList        :: [SignedPayment]
    } deriving (Generic, NFData)

instance Show ArbChannelPair where
    show (ArbChannelPair spc rpc _ _ _ _) =
        "SendState: " ++ show spc ++ "\n" ++
        "RecvState: " ++ show rpc

instance Arbitrary ArbChannelPair where
    arbitrary = fmap fst mkChanPair

instance Arbitrary ClientPayChan where
    arbitrary = fmap (sendChan . fst) mkChanPair

instance Arbitrary ServerPayChan where
    arbitrary = fmap (recvChan . fst) mkChanPair

instance Arbitrary (PayChanState BtcSig) where
    arbitrary = fmap rpcState (arbitrary :: Gen ServerPayChan)

instance Arbitrary ChanParams where
    arbitrary = fmap fst mkChanParams

instance Arbitrary FundingTxInfo where
    arbitrary = do
        ArbitraryTxHash h <- arbitrary
        i <- arbitrary
        amt <- choose (fromIntegral mIN_CHANNEL_SIZE, maxCoins)
        let fundAmount = either (error "Dust math fail") id $ mkNonDusty (fromIntegral amt :: BtcAmount)
        return $ CFundingTxInfo h i fundAmount

instance Arbitrary BtcAmount where
    arbitrary = fromIntegral <$> choose (0, maxCoins)

newtype NonZeroBitcoinAmount = NonZeroBitcoinAmount { getAmount :: BtcAmount }

instance Arbitrary NonZeroBitcoinAmount where
    arbitrary = (NonZeroBitcoinAmount . fromIntegral) <$>
        choose (1, maxCoins)

instance Arbitrary (Payment BtcSig) where
    arbitrary = snd <$> mkChanPair

instance MonadTime Gen where
    currentTime = return nowishTimestamp


toInitResult :: ArbChannelPair -> ChannelPairResult
toInitResult initPair@(ArbChannelPair spc rpc payAmt rcvAmt pay _) =
    ChannelPairResult initPair spc rpc [payAmt] [rcvAmt] [pay]


-- |Fold a payment of specified value into a 'ChannelPairResult'
doPayment :: MonadTime m => ChannelPairResult -> BtcAmount -> m ChannelPairResult
doPayment (ChannelPairResult initPair spc rpc sendList recvList payLst) amount = do
    (newSpc, pmn, amountSent) <- cappedCreatePayment spc amount
    eitherRpc <- acceptPayment rpc pmn
    case eitherRpc of
        Left e -> error (show e)
        Right (recvAmount, newRpc) -> return $
            ChannelPairResult initPair newSpc newRpc
                (amountSent : sendList)
                (recvAmount : recvList)
                (pmn : payLst)

runChanPair :: MonadTime m => ArbChannelPair -> [BtcAmount] -> m ChannelPairResult
runChanPair chanPair paymentAmountList =
    foldM doPayment (toInitResult chanPair) paymentAmountList

mkChanParams :: Gen (ChanParams, (HC.PrvKeyC, HC.PrvKeyC))
mkChanParams = do
    -- sender key pair
    ArbitraryPubKeyC sendPriv sendPK <- arbitrary
    -- receiver key pair
    ArbitraryPubKeyC recvPriv recvPK <- arbitrary
--     ArbitraryXPubKey recvPriv recvPK <- arbitrary
    -- TODO: Use a future expiration date for now
    lockTime <- fromMaybe (error "Bad lockTime") . parseLockTime <$> choose (1795556940, maxBound)
    return (MkChanParams
                (MkSendPubKey sendPK) (MkRecvPubKey recvPK) lockTime,
           (sendPriv, recvPriv))

mkChanPair :: Gen (ArbChannelPair, SignedPayment)
mkChanPair = arbitrary >>= mkChanPairInitAmount

mkChanPairInitAmount :: BtcAmount -> Gen (ArbChannelPair, SignedPayment)
mkChanPairInitAmount initPayAmount = do
    (cp, (sendPriv, recvPriv)) <- mkChanParams
    fti <- arbitrary
    sendChanE <- channelWithInitialPayment sendPriv cp fti (getFundingAddress cp) initPayAmount
    let (sendChan,initPayment) = either (error . show) id sendChanE
    recvChanE <- channelFromInitialPayment cp fti initPayment
    case recvChanE of
        Left e -> error (show e)
        Right (initRecvAmount,recvChan) -> return
             (ArbChannelPair
                                  --TODO: Cap initial payment amount
                sendChan recvChan initRecvAmount initRecvAmount initPayment recvPriv
             , initPayment)


-- TODO: We don't bother testing expiration time for now
nowishTimestamp :: UTCTime
nowishTimestamp = UTCTime (ModifiedJulianDay 50000) 0
