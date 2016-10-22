{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where


import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types
import           Data.Bitcoin.PaymentChannel.Util
import qualified Data.Bitcoin.PaymentChannel.Internal.State as S

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import           Network.Haskoin.Test
import qualified Data.Aeson         as JSON
import qualified Data.Serialize     as Bin
import           Data.Typeable

import           Data.Time.Clock                 (UTCTime(..))
import           Data.Time.Calendar              (Day(..))

import Test.QuickCheck
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- |We don't bother testing timestamps for now
nowishTimestamp :: UTCTime
nowishTimestamp = UTCTime (ModifiedJulianDay 57683) 0     -- 2016-10-22

mIN_CHANNEL_SIZE :: BitcoinAmount
mIN_CHANNEL_SIZE = cDustLimit defaultConfig * 2

testAddrTestnet :: HC.Address
testAddrTestnet = "2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469"
testAddrLivenet :: HC.Address
testAddrLivenet = "14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63"


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Payment"
        [ testProperty "Sender value in settlement tx" $
            testPaymentSession checkSenderValue
        , testProperty "Receiver value in settlement tx" $
            testPaymentSession checkReceiverValue
        , testProperty "Sender/receiver state match" $
            testPaymentSession checkSendRecvStateMatch
        , testProperty "Sent amount == received amount" $
            testPaymentSession checkRecvSendAmount
        ]
    , testGroup "Serialization"
        [ testProperty "FullPayment JSON"   (jsonSerDeser :: FullPayment -> Bool)
        , testProperty "FullPayment Binary" testPaymentBin
        -- Fails: https://github.com/haskoin/haskoin/issues/287
        -- , testProperty "ChanScript ser/deser" testScriptBin
        ]
    ]

testPaymentJSON :: FullPayment -> Bool
testPaymentJSON = jsonSerDeser
testPaymentBin :: FullPayment -> Bool
testPaymentBin = binSerDeser
testScriptBin :: ChanScript -> Bool -- Fails: https://github.com/haskoin/haskoin/issues/287
testScriptBin = binSerDeser

checkSenderValue :: (ArbChannelPair, [BitcoinAmount]) -> Bool
checkSenderValue (ArbChannelPair _ recvChan amountSent _ recvSignFunc, _) = do
    let settleTx = getSettlementBitcoinTx recvChan recvSignFunc testAddrLivenet 0
    let clientChangeAmount = HT.outValue . head . HT.txOut $ settleTx
    -- Check that the client change amount in the settlement transaction equals the
    --  channel funding amount minus the sum of all payment amounts.
    let fundAmountMinusPaySum = S.pcsChannelTotalValue (getChannelState recvChan) -
            fromIntegral (sum amountSent)
    fromIntegral clientChangeAmount == fundAmountMinusPaySum

checkReceiverValue :: (ArbChannelPair, [BitcoinAmount]) -> Bool
checkReceiverValue (ArbChannelPair _ recvChan amountSent _ recvSignFunc, _) = do
    let settleTx = getSettlementBitcoinTx recvChan recvSignFunc testAddrLivenet 0
    let receiverAmount = HT.outValue (HT.txOut settleTx !! 1)
    -- Check receiver amount in settlement transaction with zero fee equals sum
    -- of all payments.
    (fromIntegral receiverAmount :: BitcoinAmount) == fromIntegral (sum amountSent)

checkSendRecvStateMatch :: (ArbChannelPair, [BitcoinAmount]) -> Bool
checkSendRecvStateMatch (ArbChannelPair sendChan recvChan _ _ _, _) =
    getChannelState sendChan == getChannelState recvChan

checkRecvSendAmount :: (ArbChannelPair, [BitcoinAmount]) -> Bool
checkRecvSendAmount (ArbChannelPair _ _ amountSent amountRecvd _, _) =
    amountSent == amountRecvd

testPaymentSession ::
    ((ArbChannelPair, [BitcoinAmount]) -> Bool)
    -> ArbChannelPair
    -> [BitcoinAmount]
    -> Bool
testPaymentSession testFunc arbChanPair paymentAmountList =
    testFunc (runChanPair arbChanPair paymentAmountList)

runChanPair :: ArbChannelPair -> [BitcoinAmount] -> (ArbChannelPair, [BitcoinAmount])
runChanPair chanPair paymentAmountList =
    (foldl doPayment chanPair paymentAmountList, paymentAmountList)

jsonSerDeser :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a) => a -> Bool
jsonSerDeser fp =
    maybe False checkEquals decodedObj
        where json = JSON.encode fp
              decodedObj = JSON.decode json
              checkEquals serDeserVal =
                if serDeserVal /= fp then
                        error ("Ser/deser mismatch.\nOriginal: " ++ show fp ++ "\nCopy: " ++ show decodedObj)
                    else
                        True

binSerDeser :: (Typeable a, Show a, Eq a, Bin.Serialize a) => a -> Bool
binSerDeser fp =
    checkEquals decodeRes
        where bs = Bin.encode fp
              decodeRes = deserEither bs
              checkEquals serDeserRes = case serDeserRes of
                    Left e      -> error $ "Serialize/deserialize error: " ++ show e
                    Right res   -> if res /= fp then
                                    error ("Ser/deser mismatch.\nOriginal: " ++ show fp ++ "\nCopy: " ++ show res)
                                else
                                    True




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

newtype ChanScript = ChanScript HS.Script deriving (Eq,Show,Bin.Serialize)

instance Arbitrary ArbChannelPair where
    arbitrary = fmap fst mkChanPair

instance Arbitrary FullPayment where
    arbitrary = fmap snd mkChanPair

instance Arbitrary ChannelParameters where
    arbitrary = fmap fst mkChanParams

instance Arbitrary ChanScript where
    arbitrary = ChanScript . getRedeemScript <$> arbitrary

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
