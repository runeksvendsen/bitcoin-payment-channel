{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module PayChanSpec where

import           PaymentChannel.Test

import qualified Data.Aeson                  as JSON
import qualified Data.Serialize              as Bin
import qualified Network.Haskoin.Constants   as HCC
import qualified Network.Haskoin.Crypto      as HC
import qualified Network.Haskoin.Test        as HC
import qualified Network.Haskoin.Transaction as HT

import           Control.Monad.Identity      (Identity (Identity))
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic     (assert, monadic, run)
import           Test.QuickCheck.Property    (Property)

import           Debug.Trace
{-# ANN module ("HLint: ignore Redundant if"::String) #-}


newtype TestM a = TestM { runTestId :: Identity a }
    deriving (Functor, Applicative, Monad)

instance MonadTime TestM where
    currentTime = return nowishTimestamp

runTestM :: forall c. TestM c -> c
runTestM = getIt . runTestId
    where getIt (Identity a) = a

testAddr :: HC.Address
testAddr = if HCC.getNetwork == HCC.prodnet
    then "14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63"
    else "n2eMqTT929pb1RDNuqEnxdaLau1rxy3efi"

--recvSettleAddr :: HC.Address
--recvSettleAddr = testAddr

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    paymentSpec
    conversionSpec

paymentSpec :: Spec
paymentSpec =
  describe "Payment" $
    around withArbChanResult $ do
      describe "Settlement tx" $ do
        it "signatures verify" $ \res ->
            verifyTx (mkSettleBtcTx res) `shouldBe` Right ()
        it "client change amount == funding value minus sum of payment values" $ \res -> do
          let (changeAmount, fundValMinusPaySum, _) = runTestM $ checkSenderValue res
          changeAmount `shouldBe` fundValMinusPaySum
        it "receiver output amount == sum of payment values" $ \res -> do
          let (recvOutVal, paySumVal, _) = runTestM $ checkReceiverValue res
          recvOutVal `shouldBe` paySumVal
        it "always has at least one output" $ \res ->
          runTestM (minOneOutput res) `shouldBe` True
      describe "State" $ do
        it "Sender/receiver match"
          checkSendRecvStateMatch
        it "Sent amounts == received amounts"
          recvSendAmountsMatch
      describe "Channel close" $
        it "Can produce & accept arbitrary closing payment with sane fee" $ \res -> do
          HC.ArbitraryAddress arbAddr <- generate arbitrary
          txFee <- fromIntegral <$> generate (choose (0 :: Word64, 10000))
          let closedStateE = createAcceptClosingPayment arbAddr (txFee :: SatoshisPerByte) res
          closedStateE `shouldSatisfy` isRight
          let Right closedState = closedStateE
          let settleTxE = runDummy $ closedGetSettlementTxDerive closedState 0 DropDust
          settleTxE `shouldSatisfy` isRight

conversionSpec :: Spec
conversionSpec = do
  describe "Conversion works for" $
    it "RedeemScript" $ generate arbitrary >>=
      redeemScriptConversion
  describe "Serialization works for" $ do
    describe "JSON" $ do
      it "Payment" $ generate arbitrary >>=
        (jsonSerDeser :: Payment BtcSig -> IO ())
      it "ServerPayChan" $ generate arbitrary >>=
        (jsonSerDeser :: ServerPayChanX -> IO ())
    describe "Binary" $ do
      it "Payment" $ generate arbitrary >>=
        (binSerDeser  :: Payment BtcSig -> IO ())
      it "PayChanState" $ generate arbitrary >>=
        (binSerDeser  :: PayChanState BtcSig -> IO ())
      it "ChanParams" $ generate arbitrary >>=
        (binSerDeser  :: ChanParams -> IO ())

withArbChanResult :: (ChannelPairResult -> IO ()) -> IO ()
withArbChanResult f = do
    arbPair <- generate arbitrary
    amtLst  <- generate arbitrary
    runChanPair arbPair amtLst >>= f

redeemScriptConversion :: ChanParams -> IO ()
redeemScriptConversion cp =
    fromRedeemScript (getRedeemScript cp) `shouldBe` Right cp

checkSenderValue :: ChannelPairResult -> TestM (BtcAmount, BtcAmount, HT.Tx)
checkSenderValue cpr@ChannelPairResult{..} = do
    let settleTx = mkSettleTx cpr
        clientChangeOutIndex = indexOf settleTx (fundingAddress resSendChan)
        clientChangeAmount = maybe 0 (HT.outValue . (HT.txOut settleTx !!)) clientChangeOutIndex
    -- Check that the client change amount in the settlement transaction equals the
    --  channel funding amount minus the sum of all payment amounts.
    let fundingVal = fundingValue $ pcsPayment (getChanState resRecvChan)
        fundValMinusPaym = fundingVal - fromIntegral (sum resSentAmounts)
    return (fromIntegral clientChangeAmount, fundValMinusPaym, settleTx)

checkReceiverValue :: ChannelPairResult -> TestM (BtcAmount, BtcAmount, HT.Tx)
checkReceiverValue cpr@ChannelPairResult{..} = do
    let settleTx = mkSettleTx cpr
        cp = pairRedeemScript $ initPayment resInitPair
        changeAddr = getKey (detDerive (testKeyRoot $ recvPrvKey resInitPair) cp :: Internal ChildPair)
        recvOutIndex = indexOf settleTx changeAddr
        recvAmount = maybe 0 (HT.outValue . (HT.txOut settleTx !!)) recvOutIndex
    -- Check receiver amount in settlement transaction with zero fee equals sum
    -- of all payments.
    return (fromIntegral recvAmount :: BtcAmount, fromIntegral (sum resRecvdAmounts), settleTx)

minOneOutput :: ChannelPairResult -> TestM Bool
minOneOutput cpr = return . not . null . HT.txOut $ mkSettleTx cpr

checkSendRecvStateMatch :: ChannelPairResult -> IO ()
checkSendRecvStateMatch ChannelPairResult{..} =
    getChanState resSendChan `shouldBe` getChanState resRecvChan

recvSendAmountsMatch :: ChannelPairResult -> IO ()
recvSendAmountsMatch ChannelPairResult{..} =
    resSentAmounts `shouldBe` resRecvdAmounts


jsonSerDeser :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a) => a -> IO ()
jsonSerDeser fp =
    JSON.decode (JSON.encode fp) `shouldBe` Just fp

binSerDeser :: (Typeable a, Show a, Eq a, Bin.Serialize a) => a -> IO ()
binSerDeser fp =
    deserEither (Bin.encode fp) `shouldBe` Right fp


testPaymentSessionM ::
    (ChannelPairResult -> TestM Bool)
    -> ArbChannelPair
    -> [BtcAmount]
    -> Property
testPaymentSessionM testFunc arbChanPair payLst =
    monadic runTestM $
        run (runChanPair arbChanPair payLst) >>= run . testFunc >>= assert


indexOf :: HT.Tx -> HC.Address -> Maybe Int
indexOf tx addr = listToMaybe $ catMaybes $ zipWith f [0..] (HT.txOut tx)
    where f idx out =
            if HT.scriptOutput out == addressToScriptPubKeyBS addr
                then Just idx
                else Nothing

mkSettleBtcTx :: ChannelPairResult -> SettleTx
mkSettleBtcTx ChannelPairResult{..} =
    let prvKey = testKeyRoot $ recvPrvKey resInitPair in
    either (error . show) id $ runExtDet prvKey $ getSignedSettlementTx
            resRecvChan
            (mkChangeFee $ SatoshisPerByte 0, KeepDust)

mkSettleTx :: ChannelPairResult -> Tx
mkSettleTx =
     toHaskoinTx . mkSettleBtcTx
