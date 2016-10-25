{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Bitcoin.PaymentChannel.Test
import           Data.Bitcoin.PaymentChannel.Util

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Aeson         as JSON
import qualified Data.Serialize     as Bin
import           Data.Typeable

import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)


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
        [ testGroup "JSON"
            [ testProperty "FullPayment"
                (jsonSerDeser :: FullPayment -> Bool)
            ]
        , testGroup "Binary"
            [ testProperty "FullPayment"
                (binSerDeser  :: FullPayment -> Bool)
            , testProperty "PaymentChannelState"
                (binSerDeser  :: PaymentChannelState -> Bool)
            , testProperty "ChanScript"
               (binSerDeser  :: ChanScript -> Bool)
            ]
        ]
    ]


checkSenderValue :: (ArbChannelPair, [BitcoinAmount]) -> Bool
checkSenderValue (ArbChannelPair _ recvChan amountSent _ recvSignFunc, _) = do
    let settleTx = getSettlementBitcoinTx recvChan testAddrLivenet recvSignFunc (0 :: BitcoinAmount)
    let clientChangeAmount = HT.outValue . head . HT.txOut $ settleTx
    -- Check that the client change amount in the settlement transaction equals the
    --  channel funding amount minus the sum of all payment amounts.
    let fundAmountMinusPaySum = pcsChannelTotalValue (getChannelState recvChan) -
            fromIntegral (sum amountSent)
    fromIntegral clientChangeAmount == fundAmountMinusPaySum

checkReceiverValue :: (ArbChannelPair, [BitcoinAmount]) -> Bool
checkReceiverValue (ArbChannelPair _ recvChan amountSent _ recvSignFunc, _) = do
    let settleTx = getSettlementBitcoinTx recvChan testAddrLivenet recvSignFunc (0 :: BitcoinAmount)
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


