{-# LANGUAGE OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Main where

import           PaymentChannel.Test
import           PaymentChannel.Util

import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import qualified Data.Aeson                     as JSON
import qualified Data.Serialize                 as Bin
import           Data.Typeable

import Test.Framework                       (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (arbitrary)
import Test.QuickCheck.Monadic              (monadic, run , assert)
import Test.QuickCheck.Property             (Property)

import Control.Monad.Identity (Identity(Identity))

newtype TestM a = TestM { runTestId :: Identity a }
    deriving (Functor, Applicative, Monad)

instance MonadTime TestM where
    currentTime = return futureTimestamp

runTestM :: forall c. TestM c -> c
runTestM = getIt . runTestId
    where getIt (Identity a) = a

testAddrTestnet :: HC.Address
testAddrTestnet = "2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469"
testAddrLivenet :: HC.Address
testAddrLivenet = "14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63"

main :: IO ()
main = putStrLn "" >> defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Payment"
        [ testProperty "Sender value in settlement tx" $
            testPaymentSessionM checkSenderValue
        , testProperty "Receiver value in settlement tx" $
            testPaymentSessionM checkReceiverValue
        , testProperty "Sender/receiver state match" $
            testPaymentSessionM checkSendRecvStateMatch
        , testProperty "Sent amount == received amount" $
            testPaymentSessionM checkRecvSendAmount
        ]
--     , testGroup "Conversion"
--         [ testProperty "RedeemScript"
--             redeemScriptConversion
--         ]
--     , testGroup "Serialization"
--         [ testGroup "JSON"
--             [ testProperty "Payment"
--                 (jsonSerDeser :: Payment BtcSig -> Bool)
--             , testProperty "ServerPayChan"
--                 (jsonSerDeser :: ServerPayChan -> Bool)
--             ]
--         , testGroup "Binary"
--             [ testProperty "Payment"
--                 (binSerDeser  :: Payment BtcSig -> Bool)
--             , testProperty "PayChanState"
--                 (binSerDeser  :: PayChanState BtcSig -> Bool)
--             , testProperty "ChanParams"
--                 (binSerDeser  :: ChanParams -> Bool)
--             ]
--         ]
    ]

redeemScriptConversion :: ChanParams -> Bool
redeemScriptConversion cp =
    case fromRedeemScript (getRedeemScript cp) of
        Left e -> error (show cp ++ "\n\n" ++ show e)
        Right r -> r == cp || error (show cp ++ "\n\n" ++ show r)
        -- if r /= cp then error (show cp ++ "\n\n" ++ show r) else True

mkSettleTx :: Monad m =>
            ChannelPairResult -> m Tx
mkSettleTx ChannelPairResult{..} = do
    settleTxE <- getSettlementBitcoinTx
            resRecvChan testAddrLivenet
            (recvPrvKey resInitPair) (SatoshisPerByte 0)
    return $ either (error . show) id settleTxE

checkSenderValue :: ChannelPairResult -> TestM Bool
checkSenderValue cpr@ChannelPairResult{..} = do
    settleTx <- mkSettleTx cpr
    let clientChangeAmount = HT.outValue . head . HT.txOut $ settleTx
    -- Check that the client change amount in the settlement transaction equals the
    --  channel funding amount minus the sum of all payment amounts.
    let fundingVal = fundingValue $ pcsPayment (getChanState resRecvChan)
        fundAmountMinusPaySum = fundingVal - fromIntegral (sum resSendAmount)
    return $ fromIntegral clientChangeAmount == fundAmountMinusPaySum

checkReceiverValue :: ChannelPairResult -> TestM Bool
checkReceiverValue cpr@ChannelPairResult{..} = do
    settleTx <- mkSettleTx cpr
    let receiverAmount = HT.outValue (HT.txOut settleTx !! 1)
    -- Check receiver amount in settlement transaction with zero fee equals sum
    -- of all payments.
    return $ (fromIntegral receiverAmount :: BtcAmount) == fromIntegral (sum resRecvAmount)

checkSendRecvStateMatch :: ChannelPairResult -> TestM Bool
checkSendRecvStateMatch cpr@ChannelPairResult{..} =
    return $ getChanState resSendChan == getChanState resRecvChan

checkRecvSendAmount :: ChannelPairResult -> TestM Bool
checkRecvSendAmount cpr@ChannelPairResult{..} =
    return $ sum resSendAmount == sum resRecvAmount

testPaymentSessionM ::
    (ChannelPairResult -> TestM Bool)
    -> ArbChannelPair
    -> Property
testPaymentSessionM testFunc arbChanPair = monadic runTestM $ do
--     let paymentAmountList = fromIntegral <$> [2,45,23,1,22,6,4,334,3,4] :: [BtcAmount]
--     hey <- run arbitrary
    chanPairRes <- run $ runChanPair arbChanPair -- paymentAmountList
    resBool  <- run $ testFunc chanPairRes
    assert resBool

jsonSerDeser :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a) => a -> Bool
jsonSerDeser fp =
    maybe False checkEquals decodedObj
        where decodedObj = JSON.decode $ JSON.encode fp
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


