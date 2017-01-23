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
import Test.QuickCheck.Monadic              (monadic, run , assert)
import Test.QuickCheck.Property             (Property)


newtype MonadTest a = MonadTest { runTestM :: a } deriving (Functor, Applicative, Monad)

instance MonadTime MonadTest where
    currentTime = const $ return nowishTimestamp

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
--         , testProperty "Receiver value in settlement tx" $
--             testPaymentSession checkReceiverValue
--         , testProperty "Sender/receiver state match" $
--             testPaymentSession checkSendRecvStateMatch
--         , testProperty "Sent amount == received amount" $
--             testPaymentSession checkRecvSendAmount
        ]
    , testGroup "Conversion"
        [ testProperty "RedeemScript"
            redeemScriptConversion
        ]
    , testGroup "Serialization"
        [ testGroup "JSON"
            [ testProperty "Payment"
                (jsonSerDeser :: Payment BtcSig -> Bool)
            , testProperty "ServerPayChan"
                (jsonSerDeser :: ServerPayChan -> Bool)
--             , testProperty "ClientPayChan"
--                 (jsonSerDeser :: ClientPayChan -> Bool)
            ]
        , testGroup "Binary"
            [ testProperty "Payment"
                (binSerDeser  :: Payment BtcSig -> Bool)
            , testProperty "PayChanState"
                (binSerDeser  :: PayChanState BtcSig -> Bool)
            , testProperty "ChanParams"
                (binSerDeser  :: ChanParams -> Bool)
            ]
        ]
    ]

redeemScriptConversion :: ChanParams -> Bool
redeemScriptConversion cp =
    case fromRedeemScript (getRedeemScript cp) of
        Left e -> error (show cp ++ "\n\n" ++ show e)
        Right r -> r == cp || error (show cp ++ "\n\n" ++ show r)
        -- if r /= cp then error (show cp ++ "\n\n" ++ show r) else True

checkSenderValue :: (ChannelPairResult, [BtcAmount]) -> MonadTest Bool
checkSenderValue (ChannelPairResult{..}, _) = do
    settleTxE <- getSettlementBitcoinTx resRecvChan testAddrLivenet
            (recvPrvKey resInitPair) (SatoshisPerByte 0)
    let settleTx = either (error . show) id settleTxE
    let clientChangeAmount = HT.outValue . head . HT.txOut $ settleTx
    -- Check that the client change amount in the settlement transaction equals the
    --  channel funding amount minus the sum of all payment amounts.
    let fundingVal = fundingValue $ pcsPayment (getChanState resRecvChan)
        fundAmountMinusPaySum = fundingVal - fromIntegral (sum resSendAmount)
    return $ fromIntegral clientChangeAmount == fundAmountMinusPaySum

-- checkReceiverValue :: (ChannelPairResult, [BtcAmount]) -> Bool
-- checkReceiverValue (ChannelPairResult{..}, _) = do
--     let settleTx = getSettlementBitcoinTx resRecvChan testAddrLivenet
--             (recvSignFunc resInitPair) (0 :: BtcAmount)
--     let receiverAmount = HT.outValue (HT.txOut settleTx !! 1)
--     -- Check receiver amount in settlement transaction with zero fee equals sum
--     -- of all payments.
--     (fromIntegral receiverAmount :: BtcAmount) == fromIntegral (sum resRecvAmount)

checkSendRecvStateMatch :: (ChannelPairResult, [BtcAmount]) -> Bool
checkSendRecvStateMatch (ChannelPairResult{..}, _) =
    getChanState resSendChan == getChanState resRecvChan

checkRecvSendAmount :: (ChannelPairResult, [BtcAmount]) -> Bool
checkRecvSendAmount (ChannelPairResult{..},_) =
    sum resSendAmount == sum resRecvAmount

testPaymentSession :: MonadTime m =>
    ((ChannelPairResult, [BtcAmount]) -> m Bool)
    -> ArbChannelPair
    -> [BtcAmount]
    -> Property
testPaymentSession testFunc arbChanPair paymentAmountList = monadic $ do
    chanPair <- run $ runChanPair arbChanPair paymentAmountList
    resBool  <- run $ testFunc chanPair
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


