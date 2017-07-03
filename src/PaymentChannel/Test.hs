{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
module PaymentChannel.Test
( module PaymentChannel.Test
, module X
)   where

import PaymentChannel           as X
import PaymentChannel.Util      as X
import PaymentChannel.Internal.Receiver.Types as X
import qualified RBPCP.Types as RBPCP

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Script as HS
import qualified Data.ByteString.Char8        as C8
import qualified Data.Serialize         as Bin
import           Network.Haskoin.Test
import            Data.Time.Clock.POSIX
import           Data.Time.Clock                 (UTCTime(..))
import           Data.Time.Calendar              (Day(..))
import           Control.Monad                   (foldM)

import Test.QuickCheck

import Debug.Trace
debugEnable = False
debugTrace str a = if debugEnable then trace str a else a

maxCoins :: Integer
maxCoins = fromIntegral (maxBound :: BtcAmount)

mIN_CHANNEL_SIZE :: BtcAmount
mIN_CHANNEL_SIZE = testDustLimit * 2

testDustLimit :: BtcAmount
testDustLimit = 6200

testSettlePeriod :: Hour
testSettlePeriod = MkHour 12

testMinDuration :: Hour
testMinDuration = MkHour 48

mkTestServerConf :: BtcAmount -> ServerSettings
mkTestServerConf = ServerSettings
    testDustLimit testSettlePeriod testMinDuration


data ArbChannelPair = ArbChannelPair
    { sendChan          :: ClientPayChan
    , recvChan          :: ServerPayChanX
    , initPayAmount     :: BtcAmount
    , initRecvAmount    :: BtcAmount
    , initPayment       :: SignedPayment
    , recvPrvKey        :: TestRecvKey
    } deriving (Generic, NFData)

data TestRecvKey = TestRecvKey RootPrv (External ChildPair)
      deriving (Generic, NFData)

testPrvKeyC :: TestRecvKey -> HC.PrvKeyC
testPrvKeyC (TestRecvKey _ pair) = getKey pair

data ChannelPairResult = ChannelPairResult
    { resInitPair       :: ArbChannelPair
    , resSendChan       :: ClientPayChan
    , resRecvChan       :: ServerPayChanX
    , resSentAmounts    :: [BtcAmount]
    , resRecvdAmounts   :: [BtcAmount]
    , resPayList        :: [SignedPayment]
    } deriving (Generic, NFData, Show)

instance Show ArbChannelPair where
    show (ArbChannelPair spc rpc _ _ _ _) =
        "SendState: " ++ show spc ++ "\n" ++
        "RecvState: " ++ show rpc

instance Arbitrary ArbChannelPair where
    arbitrary = fmap fst mkChanPair

instance Arbitrary ClientPayChan where
    arbitrary = fmap (sendChan . fst) mkChanPair

instance Arbitrary ServerPayChanX where
    arbitrary = fmap (recvChan . fst) mkChanPair

instance Arbitrary (PayChanState BtcSig) where
    arbitrary = fmap rpcState (arbitrary :: Gen ServerPayChanX)

instance Arbitrary ChanParams where
    arbitrary = fmap fst mkChanParams

instance Arbitrary BtcAmount where
    arbitrary = fromIntegral <$> choose (0, maxCoins)

instance Arbitrary NonDustyAmount where
    arbitrary = arbitraryNonDusty 0

arbitraryNonDusty :: BtcAmount -> Gen NonDustyAmount
arbitraryNonDusty extraVal = do
  val <- fromIntegral <$> choose (fromIntegral $ testDustLimit + extraVal, maxCoins)
  either (\e -> error $ "Dusty amount: " ++ show val) return $
      runConfM (mkTestServerConf 0) $ mkNonDusty (val :: BtcAmount)

genLockTimeDate
    :: ServerSettings
    -> UTCTime          -- ^ Now-timestamp
    -> Hour             -- ^ Maximum duration
    -> Gen LockTimeDate
genLockTimeDate ServerSettings{..} now maxDuration = do
    let leeway = 6 :: Hour
        maxTs = fromIntegral (maxBound :: Word32)
        startTs = (round $ utcTimeToPOSIXSeconds now :: Integer) +
                    toSeconds (serverConfSettlePeriod + serverConfMinDuration + leeway)
    timestamp <- choose (startTs, min (startTs + toSeconds maxDuration) maxTs)
    either (const $ error $ "genLockTimeDate: bad logic: " ++ show timestamp) return $
        parseLockTime (fromIntegral timestamp)


newtype NonZeroBitcoinAmount = NonZeroBitcoinAmount { getAmount :: BtcAmount }

instance Arbitrary NonZeroBitcoinAmount where
    arbitrary = (NonZeroBitcoinAmount . fromIntegral) <$>
        choose (1, maxCoins)

instance Arbitrary (Payment BtcSig) where
    arbitrary = snd <$> mkChanPair

-- -- Soft child keys (keys derivable from an XPubKey) have an index of less than 0x80000000
--instance Arbitrary KeyDeriveIndex where
--    arbitrary =
--        fromMaybe (error "Bad key index") . mkKeyIndex <$> choose (0, 0x80000000 - 1)

instance MonadTime Gen where
    currentTime = return nowishTimestamp


instance Arbitrary RootPrv where
     arbitrary = createRootPrv <$> arbitrary

instance Arbitrary ByteString where
    arbitrary = do
        len <- choose (0,32)
        c8Lst <- vector len
        return $ C8.pack c8Lst


toInitResult :: ArbChannelPair -> ChannelPairResult
toInitResult initPair@(ArbChannelPair spc rpc payAmt rcvAmt pay _) =
    ChannelPairResult initPair spc rpc [payAmt] [rcvAmt] [pay]


-- |Fold a payment of specified value into a 'ChannelPairResult'
doPayment :: MonadTime m => ChannelPairResult -> BtcAmount -> m ChannelPairResult
doPayment (ChannelPairResult initPair spc rpc sendList recvList payLst) amount = do
    let (newSpc, pmn, amountSent) = createPaymentCapped spc (Capped amount)
    eitherRpc <- ("doPayment send: " ++ show amountSent) `debugTrace` acceptPayment (toPaymentData pmn) rpc
    case eitherRpc of
        Left e -> error (show e)
        Right (newRpc, recvAmount) -> return $ ("doPayment recv: " ++ show recvAmount) `debugTrace`
            ChannelPairResult initPair newSpc newRpc
                (amountSent : sendList)
                (recvAmount : recvList)
                (pmn : payLst)

runChanPair :: MonadTime m => ArbChannelPair -> [BtcAmount] -> m ChannelPairResult
runChanPair chanPair paymentAmountList =
    ("runChanPair lst: " ++ show paymentAmountList) `debugTrace`
    foldM doPayment (toInitResult chanPair) paymentAmountList

mkChanParams :: Gen (ChanParams, (HC.PrvKeyC, TestRecvKey))
mkChanParams = arbitrary >>= fromRecvRootKey

fromRecvRootKey :: RootPrv -> Gen (ChanParams, (HC.PrvKeyC, TestRecvKey))
fromRecvRootKey recvRoot = do
    -- sender key pair
    ArbitraryPubKeyC sendPriv sendPK <- arbitrary
    -- receiver key pair
    ArbitrarySoftPath arbPath <- arbitrary
    let childPair = mkChild recvRoot arbPath :: External ChildPair
        recvPK    = getKey childPair
    -- TODO: Use a future expiration date for now
    lockTime <- either (error "Bad lockTime") id . parseLockTime <$> choose (1795556940, maxBound)
    return (ChanParams
                (MkSendPubKey sendPK) (MkRecvPubKey $ HC.xPubKey recvPK) lockTime,
           (sendPriv, TestRecvKey recvRoot childPair ))

mkChanPair :: Gen (ArbChannelPair, SignedPayment)
mkChanPair = arbitrary >>= mkChanPairInitAmount

mkChanPairInitAmount :: BtcAmount -> Gen (ArbChannelPair, SignedPayment)
mkChanPairInitAmount initPayAmount = do
    let testServerConf = mkTestServerConf initPayAmount
    (cp, (sendPriv, recvKey@(TestRecvKey _ childPair))) <- mkChanParams
    fundingVal <- arbitraryNonDusty $ max mIN_CHANNEL_SIZE (initPayAmount + testDustLimit)
    (vout,tx)  <- arbitraryFundingTx cp (nonDusty fundingVal)
    let fundInfo = testRbpcpFundingInfo testServerConf cp initPayAmount
        sendChanE = channelWithInitialPayment sendPriv (cpLockTime cp) (tx,vout) fundInfo
    let (sendChan,initPayment) = either (error . show) id sendChanE

    recvChanE <- channelFromInitialPayment testServerConf tx (toPaymentData initPayment)
    let mkExtRPC chan = fromMaybe (error $ "mkExtendedKeyRPC failed. " ++ show (chan,childPair))
                                  $ mkExtendedKeyRPC chan (fromExternalPair childPair)
    case recvChanE of
        Left e -> error (show e)
        Right recvChan -> return
                 ( ArbChannelPair
                    sendChan (mkExtRPC recvChan) initPayAmount initPayAmount initPayment recvKey
                 , initPayment)

testRbpcpFundingInfo ::
       ServerSettings
    -> ChanParams
    -> BtcAmount          -- ^ Open price
    -> RBPCP.FundingInfo
testRbpcpFundingInfo ServerSettings{..} cp openPrice =
    RBPCP.FundingInfo
        { RBPCP.fundingInfoServerPubkey               = RBPCP.Server . getPubKey . getRecvPubKey $ cp
        , RBPCP.fundingInfoDustLimit                  = fromIntegral testDustLimit
        , RBPCP.fundingInfoFundingAddressCopy         = getFundingAddress cp
        , RBPCP.fundingInfoOpenPrice                  = fromIntegral openPrice
        , RBPCP.fundingInfoFundingTxMinConf        = 0
        , RBPCP.fundingInfoSettlementPeriodHours    = fromIntegral serverConfSettlePeriod
        , RBPCP.fundingInfoMinDurationHours         = fromIntegral serverConfMinDuration
        }

-- Arbitrary range
--instance Arbitrary (BtcAmount,BtcAmount) where
--    arbitrary = do
--        arbMin <- arbitrary
--        arbMax <- choose (fromIntegral arbMin, maxCoins)
--        return (arbMin, fromIntegral arbMax)

genRunChanPair :: Word -> (BtcAmount,BtcAmount) -> BtcAmount -> IO ChannelPairResult
genRunChanPair numPayments (rangeMin,rangeMax) initAmount = do
    amountList <- fmap (map conv) <$> generate $
        vectorOf (conv numPayments) (choose (conv rangeMin, conv rangeMax) :: Gen Word64)
    (arbPair,_) <- generate $ mkChanPairInitAmount initAmount
    runChanPair arbPair amountList
  where
    conv :: (Integral a, Num b) => a -> b
    conv = fromIntegral

-- | Funding transaction with a funding output at an arbitrary output index
arbitraryFundingTx
    :: ChanParams
    -> BtcAmount        -- ^ Funding amount
    -> Gen (Word32, Tx) -- ^ Output index of funding output plus transaction
arbitraryFundingTx cp val = do
    ArbitraryTx tx <- arbitrary
    let mkP2shOut = Bin.encode . HS.encodeOutput . HS.PayScriptHash
        out = HT.TxOut (fromIntegral val) (mkP2shOut $ getP2SHFundingAddress cp)
    (idx,newOuts) <- arbitraryInsert (HT.txOut tx) out
    let newTx = HT.createTx (HT.txVersion tx) (HT.txIn tx) newOuts (HT.txLockTime tx)
    return (fromIntegral idx :: Word32, newTx)

-- | Insert an element into a list at an arbitrary position
arbitraryInsert :: forall a. [a] -> a -> Gen (Int, [a])
arbitraryInsert lst a = do
    idx <- choose (0, length lst)
    let (preLst,postLst) = splitAt idx lst
        newLst = preLst ++ [a] ++ postLst
    return (idx, newLst)

-- TODO: We don't bother testing expiration time for now
nowishTimestamp :: UTCTime
nowishTimestamp = UTCTime (ModifiedJulianDay 50000) 0


createAcceptClosingPayment
    :: ChangeOutFee fee
    => HC.Address
    -> fee
    -> ChannelPairResult
    -> Either PayChanError ClosedServerChanX
createAcceptClosingPayment addr fee ChannelPairResult{..} =
    resultFromThePast $ acceptClosingPayment (toPaymentData closingPayment) resRecvChan
  where
    (_,closingPayment,_) = createClosingPayment resSendChan addr fee
