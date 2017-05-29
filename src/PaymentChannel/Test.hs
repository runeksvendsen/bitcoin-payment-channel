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

import qualified Data.Serialize         as Bin
import           Network.Haskoin.Test
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

data TestRecvKey = TestRecvKey RootKey (External ChildPair) KeyDeriveIndex
      deriving (Generic, NFData)

testPrvKeyC :: TestRecvKey -> HC.PrvKeyC
testPrvKeyC (TestRecvKey _ pair kdi) = subKey pair kdi

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

newtype NonZeroBitcoinAmount = NonZeroBitcoinAmount { getAmount :: BtcAmount }

instance Arbitrary NonZeroBitcoinAmount where
    arbitrary = (NonZeroBitcoinAmount . fromIntegral) <$>
        choose (1, maxCoins)

instance Arbitrary (Payment BtcSig) where
    arbitrary = snd <$> mkChanPair

-- Soft child keys (keys derivable from an XPubKey) have an index of less than 0x80000000
instance Arbitrary KeyDeriveIndex where
    arbitrary =
        fromMaybe (error "Bad key index") . mkKeyIndex <$> choose (0, 0x80000000 - 1)

instance MonadTime Gen where
    currentTime = return nowishTimestamp


toInitResult :: ArbChannelPair -> ChannelPairResult
toInitResult initPair@(ArbChannelPair spc rpc payAmt rcvAmt pay _) =
    ChannelPairResult initPair spc rpc [payAmt] [rcvAmt] [pay]


-- |Fold a payment of specified value into a 'ChannelPairResult'
doPayment :: MonadTime m => ChannelPairResult -> BtcAmount -> m ChannelPairResult
doPayment (ChannelPairResult initPair spc rpc sendList recvList payLst) amount = do
    let (newSpc, pmn, amountSent) = createPayment spc (Capped amount)
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

fromRecvRootKey :: RootKey -> Gen (ChanParams, (HC.PrvKeyC, TestRecvKey))
fromRecvRootKey recvRoot = do
    -- sender key pair
    ArbitraryPubKeyC sendPriv sendPK <- arbitrary
    -- receiver key pair
    keyDerivIdx <- arbitrary
    let childPair = mkChild recvRoot :: External ChildPair
        recvPK    = subKey childPair keyDerivIdx
    -- TODO: Use a future expiration date for now
    lockTime <- either (error "Bad lockTime") id . parseLockTime <$> choose (1795556940, maxBound)
    return (MkChanParams
                (MkSendPubKey sendPK) (MkRecvPubKey $ HC.xPubKey recvPK) lockTime,
           (sendPriv, TestRecvKey recvRoot childPair keyDerivIdx))


mkChanPair :: Gen (ArbChannelPair, SignedPayment)
mkChanPair = arbitrary >>= mkChanPairInitAmount

mkChanPairInitAmount :: BtcAmount -> Gen (ArbChannelPair, SignedPayment)
mkChanPairInitAmount initPayAmount = do
    let testServerConf = mkTestServerConf initPayAmount
    (cp, (sendPriv, recvKey@(TestRecvKey _ childPair keyDerivIdx))) <- mkChanParams
    let recvXPub = subKey childPair keyDerivIdx
    fundingVal <- arbitraryNonDusty $ max mIN_CHANNEL_SIZE (initPayAmount + testDustLimit)
    (vout,tx)  <- arbitraryFundingTx cp (nonDusty fundingVal)
    let fundInfo = rbpcpFundingInfo testServerConf cp initPayAmount
        sendChanE = channelWithInitialPayment sendPriv (cpLockTime cp) (tx,vout) fundInfo
    let (sendChan,initPayment) = either (error . show) id sendChanE

    recvChanE <- channelFromInitialPayment testServerConf tx (toPaymentData initPayment)
    let mkExtRPC chan = fromMaybe (error "mkExtendedKeyRPC failed")
                                  $ mkExtendedKeyRPC chan recvXPub
    case recvChanE of
        Left e -> error (show e)
        Right recvChan -> return $
                 ( ArbChannelPair
                    sendChan (mkExtRPC recvChan) initPayAmount initPayAmount initPayment recvKey
                 , initPayment)

rbpcpFundingInfo ::
       ServerSettings
    -> ChanParams
    -> BtcAmount          -- ^ Open price
    -> RBPCP.FundingInfo
rbpcpFundingInfo ServerSettings{..} cp openPrice =
    RBPCP.FundingInfo
        (RBPCP.Server . getPubKey . getRecvPubKey $ cp)
        (fromIntegral testDustLimit)
        (getFundingAddress cp)
        (fromIntegral openPrice)
        0
        (fromIntegral serverConfSettlePeriod)
        (fromIntegral serverConfMinDuration)

{-
FundingInfo
    { fundingInfoServerPubkey               :: Server PubKey    -- ^ Server/value receiver public key. Hex-encoded, compressed Secp256k1 pubkey, 33 bytes.
    , fundingInfoDustLimit                  :: Word64  -- ^ (Satoshis) The server will not accept payments where the client change amount is less than this amount. This \"dust limit\" is necessary in order to avoid producing a settlement transaction that will not circulate in the Bitcoin P2P network because it contains an output of minuscule value. Consequently, the maximum amount, that can be sent over the payment channel, is the amount sent to the funding address minus this \"dust limit\".
    , fundingInfoFundingAddressCopy         :: Address    -- ^ Server derived channel funding address. The client will confirm that its own derived funding address matches this one, before paying to it.
    , fundingInfoOpenPrice                  :: Word64 -- ^ Price (in satoshis) for opening a channel with the given {exp_time}. This amount is paid in the initial channel payment when creating a new channel. May be zero, in which case a payment of zero value is transferred, ensuring that the channel can be closed at any time.
    , fundingInfoFunding_tx_min_conf        :: BtcConf -- ^ Minimum confirmation count that the funding transaction must have before proceeding with opening a new channel.
    , fundingInfoSettlement_period_hours    :: Hours -- ^ The server reserves the right to close the payment channel this many hours before the specified expiration date. The server hasn't received any actual value until it publishes a payment transaction to the Bitcoin network, so it needs a window of time in which the client can no longer send payments over the channel, and yet the channel refund transaction hasn't become valid.
    , fundingInfoMin_duration_hours         :: Hours -- ^ Minimum duration of newly opened channels
    }
-}


-- Arbitrary range
instance Arbitrary (BtcAmount,BtcAmount) where
    arbitrary = do
        arbMin <- arbitrary
        arbMax <- choose (fromIntegral arbMin, maxCoins)
        return (arbMin, fromIntegral arbMax)

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
    -> BtcAmount
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

-- Key stuff
instance Arbitrary RootKey where
    arbitrary = (\(ArbitraryXPrvKey k) -> fromRootPrv k) <$> arbitrary

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
