{-# LANGUAGE OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Main where

import           PaymentChannel.Test

import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import qualified Network.Haskoin.Test           as HC
import qualified Data.Aeson                     as JSON
import qualified Data.Serialize                 as Bin

import Test.QuickCheck
import Test.QuickCheck.Monadic              (monadic, run , assert)
import Test.QuickCheck.Property             (Property)
import Control.Monad.Identity (Identity(Identity))
import Test.Hspec

import Debug.Trace
{-# ANN module ("HLint: ignore Redundant if"::String) #-}


newtype TestM a = TestM { runTestId :: Identity a }
    deriving (Functor, Applicative, Monad)

instance MonadTime TestM where
    currentTime = return nowishTimestamp

runTestM :: forall c. TestM c -> c
runTestM = getIt . runTestId
    where getIt (Identity a) = a

-- testAddrTestnet :: HC.Address
-- testAddrTestnet = "2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469"
testAddrLivenet :: HC.Address
testAddrLivenet = "14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63"

recvSettleAddr :: HC.Address
recvSettleAddr = testAddrLivenet

main :: IO ()
main = hspec $ do
    paymentSpec
    conversionSpec
    keySpec

paymentSpec :: Spec
paymentSpec =
  describe "Payment" $
    around withArbChanResult $ do
      describe "Settlement tx" $ do
        it "client change output amount equals funding value minus sum of payment values" $ \res -> do
          let (changeAmount, fundValMinusPaySum, _) = runTestM $ checkSenderValue res
          changeAmount `shouldBe` fundValMinusPaySum
        it "receiver output amount equals sum of payment values" $ \res -> do
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
          HC.ArbitraryAddress arbServerAddr <- generate arbitrary
          HC.ArbitraryPrvKeyC prvKey <- generate arbitrary
          let Identity settleTxE = closedGetSettlementTx
                closedState arbServerAddr (const $ return prvKey) DropDust
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

keySpec :: Spec
keySpec  = do
  describe "External ChildPub/ChildPair" $
    it "derive same pubkeys & created signatures verify" $ do
      rootKey  <- generate arbitrary
      keyIndex <- generate arbitrary
      let pair :: External ChildPair
          pair = mkChild rootKey
          pub :: External ChildPub
          pub  = mkChild rootKey
      (subKey pair keyIndex :: HC.XPubKey) `shouldBe` (subKey pub keyIndex :: HC.XPubKey)
      HC.ArbitraryHash256 h256 <- generate arbitrary
      -- External pair/pub compare
      HC.signMsg h256 (subKey pair keyIndex :: HC.PrvKeyC) `shouldSatisfy`
          (\sig -> HC.verifySig h256 sig (subKey pub keyIndex :: HC.PubKeyC))
      -- External pair/pair compare
      HC.signMsg h256 (subKey pair keyIndex :: HC.PrvKeyC) `shouldSatisfy`
          (\sig -> HC.verifySig h256 sig (subKey pair keyIndex :: HC.PubKeyC))
  describe "Internal ChildPair" $
    it "created signatures verify" $ do
      rootKey  <- generate arbitrary
      keyIndex <- generate arbitrary
      let pair :: Internal ChildPair
          pair = mkChild rootKey
      HC.ArbitraryHash256 h256 <- generate arbitrary
      -- Internal pair/pair compare
      HC.signMsg h256 (subKey pair keyIndex :: HC.PrvKeyC) `shouldSatisfy`
          (\sig -> HC.verifySig h256 sig (subKey pair keyIndex :: HC.PubKeyC))


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
    settleTx <- mkSettleTx cpr
    let clientChangeOutIndex = indexOf settleTx (fundingAddress resSendChan)
        clientChangeAmount = maybe 0 (HT.outValue . (HT.txOut settleTx !!)) clientChangeOutIndex
    -- Check that the client change amount in the settlement transaction equals the
    --  channel funding amount minus the sum of all payment amounts.
    let fundingVal = fundingValue $ pcsPayment (getChanState resRecvChan)
        fundValMinusPaym = fundingVal - fromIntegral (sum resSentAmounts)
    return (fromIntegral clientChangeAmount, fundValMinusPaym, settleTx)

checkReceiverValue :: ChannelPairResult -> TestM (BtcAmount, BtcAmount, HT.Tx)
checkReceiverValue cpr@ChannelPairResult{..} = do
    settleTx <- mkSettleTx cpr
    let recvOutIndex = indexOf settleTx recvSettleAddr
        recvAmount = maybe 0 (HT.outValue . (HT.txOut settleTx !!)) recvOutIndex
    -- Check receiver amount in settlement transaction with zero fee equals sum
    -- of all payments.
    return (fromIntegral recvAmount :: BtcAmount, fromIntegral (sum resRecvdAmounts), settleTx)

minOneOutput :: ChannelPairResult -> TestM Bool
minOneOutput cpr = not . null . HT.txOut <$> mkSettleTx cpr

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

mkSettleTx :: Monad m =>
            ChannelPairResult -> m Tx
mkSettleTx ChannelPairResult{..} = do
    settleTxE <- getSettlementBitcoinTx
            resRecvChan recvSettleAddr
            (const $ return $ testPrvKeyC $ recvPrvKey resInitPair) (SatoshisPerByte 0) KeepDust
    return $ either (error . show) id settleTxE
