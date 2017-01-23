{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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
    }

data ChannelPairResult = ChannelPairResult
    { resInitPair   :: ArbChannelPair
    , resSendChan   :: ClientPayChan
    , resRecvChan   :: ServerPayChan
    , resSendAmount :: [BtcAmount]
    , resRecvAmount :: [BtcAmount]
    , resPayList    :: [SignedPayment]
    }

toInitResult :: ArbChannelPair -> ChannelPairResult
toInitResult initPair@(ArbChannelPair spc rpc amt rcv pay _) =
    ChannelPairResult initPair spc rpc [amt] [rcv] [pay]

instance Show ArbChannelPair where
    show (ArbChannelPair spc rpc _ _ _ _) =
        "SendState: " ++ show spc ++ "\n" ++
        "RecvState: " ++ show rpc

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

runChanPair :: MonadTime m => ArbChannelPair -> [BtcAmount] -> m (ChannelPairResult, [BtcAmount])
runChanPair chanPair paymentAmountList =
    foldM doPayment (toInitResult chanPair) paymentAmountList >>=
        \res -> return (res, paymentAmountList)

instance Arbitrary ArbChannelPair where
    arbitrary = fmap fst mkChanPair

instance Arbitrary ChannelPairResult where
    arbitrary = do
        chanPair <- arbitrary
        amtLst   <- arbitrary
        fst <$> runChanPair chanPair amtLst

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
        let nonDustyMinBound = max (fromIntegral mIN_CHANNEL_SIZE) configDustLimit
        amt <- fromIntegral <$> choose (fromIntegral nonDustyMinBound, maxCoins)
        let fundAmount = either (error "Dust math fail") id $ mkNonDusty amt
        return $ CFundingTxInfo h i fundAmount

instance Arbitrary BtcAmount where
    arbitrary = fromIntegral <$> choose (0, maxCoins)

newtype NonZeroBitcoinAmount = NonZeroBitcoinAmount { getAmount :: BtcAmount }

instance Arbitrary NonZeroBitcoinAmount where
    arbitrary = (NonZeroBitcoinAmount . fromIntegral) <$>
        choose (1, round $ ((21e6 :: Double) :: Double) * 1e8 :: Integer)

instance Arbitrary (Payment BtcSig) where
    arbitrary = snd <$> (arbitrary >>= mkChanPairInitAmount)

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
    -- create states
    sendChanE <- channelWithInitialPayment sendPriv cp fti (getFundingAddress cp) initPayAmount
    --TODO: Cap initial payment
    let (sendChan,initPayment) = either (error . show) id sendChanE
    recvChanE <- channelFromInitialPayment cp fti initPayment
    case recvChanE of
        Left e -> error (show e)
        Right (initRecvAmount,recvChan) -> return
             (ArbChannelPair
                sendChan recvChan initPayAmount initRecvAmount initPayment recvPriv
             , initPayment)


-- TODO: We don't bother testing expiration time for now
nowishTimestamp :: UTCTime
nowishTimestamp = UTCTime (ModifiedJulianDay 57683) 0     -- 2016-10-22

instance MonadTime Gen where
    currentTime = return nowishTimestamp
