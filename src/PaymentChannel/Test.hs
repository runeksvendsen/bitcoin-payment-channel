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
import PaymentChannel.Internal.Config

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
maxCoins = round $ (21e6 :: Double) * 1e8

mIN_CHANNEL_SIZE :: BtcAmount
mIN_CHANNEL_SIZE = configDustLimit * 2


data ArbChannelPair = ArbChannelPair
    { sendChan          :: ClientPayChan
    , recvChan          :: ServerPayChanX
    , initPayAmount     :: BtcAmount
    , initRecvAmount    :: BtcAmount
    , initPayment       :: SignedPayment
    , recvPrvKey        :: HC.PrvKeyC
    } deriving (Generic, NFData)

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

--instance Arbitrary FundingTxInfo where
--    arbitrary = do
--        ArbitraryTxHash h <- arbitrary
--        i <- arbitrary
--        amt <- choose (fromIntegral mIN_CHANNEL_SIZE, maxCoins)
--        let fundAmount = either (error "Dust math fail") id $ mkNonDusty (fromIntegral amt :: BtcAmount)
--        return $ CFundingTxInfo h i fundAmount

instance Arbitrary BtcAmount where
    arbitrary = fromIntegral <$> choose (0, maxCoins)

instance Arbitrary (NonDusty BtcAmount) where
    arbitrary = arbitraryNonDusty 0

arbitraryNonDusty extraVal = do
  val <- fromIntegral <$> choose (fromIntegral $ getDustLimit + extraVal, maxCoins)
  either (\e -> error $ "Dusty amount: " ++ show val) return $
      mkNonDusty (val :: BtcAmount)

newtype NonZeroBitcoinAmount = NonZeroBitcoinAmount { getAmount :: BtcAmount }

instance Arbitrary NonZeroBitcoinAmount where
    arbitrary = (NonZeroBitcoinAmount . fromIntegral) <$>
        choose (1, maxCoins)

instance Arbitrary (Payment BtcSig) where
    arbitrary = snd <$> mkChanPair

-- Hard sub keys have an index of, at most, 0x80000000
instance Arbitrary KeyDeriveIndex where
    arbitrary = fromMaybe (error "Bad key index") . mkKeyIndex <$> choose (0, 0x80000000)

instance MonadTime Gen where
    currentTime = return nowishTimestamp


toInitResult :: ArbChannelPair -> ChannelPairResult
toInitResult initPair@(ArbChannelPair spc rpc payAmt rcvAmt pay _) =
    ChannelPairResult initPair spc rpc [payAmt] [rcvAmt] [pay]


-- |Fold a payment of specified value into a 'ChannelPairResult'
doPayment :: MonadTime m => ChannelPairResult -> BtcAmount -> m ChannelPairResult
doPayment (ChannelPairResult initPair spc rpc sendList recvList payLst) amount = do
    (newSpc, pmn, amountSent) <- cappedCreatePayment spc amount
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

mkChanParams :: Gen (ChanParams, (HC.PrvKeyC, HC.PrvKeyC, HC.XPubKey))
mkChanParams = do
    -- sender key pair
    ArbitraryPubKeyC sendPriv sendPK <- arbitrary
    -- receiver key pair
--     ArbitraryPubKeyC recvPriv recvPK <- arbitrary
    kdi <- arbitrary
    ArbitraryXPrvKey recvXPrv <- arbitrary
    let (recvPriv, recvPK) = subKey recvXPrv kdi
--     ArbitraryXPubKey recvPriv recvPK <- arbitrary
    -- TODO: Use a future expiration date for now
    lockTime <- fromMaybe (error "Bad lockTime") . parseLockTime <$> choose (1795556940, maxBound)
    return (MkChanParams
                (MkSendPubKey sendPK) (MkRecvPubKey $ HC.xPubKey recvPK) lockTime,
           (sendPriv, recvPriv, recvPK))

subKey :: HC.XPrvKey -> KeyDeriveIndex -> (HC.PrvKeyC, HC.XPubKey)
subKey prv kdi = do
    let hardSub = HC.hardSubKey prv (word32Index kdi)
    let mkKeyPair k = (HC.xPrvKey k, HC.deriveXPubKey k)
    mkKeyPair hardSub


mkChanPair :: Gen (ArbChannelPair, SignedPayment)
mkChanPair = arbitrary >>= mkChanPairInitAmount

mkChanPairInitAmount :: BtcAmount -> Gen (ArbChannelPair, SignedPayment)
mkChanPairInitAmount initPayAmount = do
    (cp, (sendPriv, recvPriv, recvXPub)) <- mkChanParams
    fundingVal <- arbitraryNonDusty $ max mIN_CHANNEL_SIZE (initPayAmount + configDustLimit)
    (vout,tx)  <- arbitraryFundingTx cp (nonDusty fundingVal)
    let fti = ("fundingVal: " ++ show fundingVal) `debugTrace` CFundingTxInfo (HT.txHash tx) vout fundingVal

    sendChanE <- channelWithInitialPayment sendPriv cp fti (getFundingAddress cp) initPayAmount
    let (sendChan,initPayment) = either (error . show) id sendChanE

    recvChanE <- channelFromInitialPayment tx (toPaymentData initPayment)
    let mkExtRPC chan = fromMaybe (error "mkExtendedKeyRPC failed")
                                  $ mkExtendedKeyRPC chan recvXPub
    case recvChanE of
        Left e -> error (show e)
        Right (recvChan, initRecvAmount) -> return $
             ("mkChanPair: " ++ show (initPayAmount, initRecvAmount)) `debugTrace`
                 (ArbChannelPair
                    sendChan (mkExtRPC recvChan) initPayAmount initRecvAmount initPayment recvPriv
                 , initPayment)

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
