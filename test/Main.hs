{-# LANGUAGE OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Main where

import           PaymentChannel.Test
import           PaymentChannel.Util
import           Bitcoin.SpendCond.Util

import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import qualified Data.Aeson                     as JSON
import qualified Data.Serialize                 as Bin
import           Data.Typeable
import           Data.Maybe
import           Data.Either

import Test.Framework                       (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      -- (arbitrary)
import Test.QuickCheck.Monadic              (monadic, run , assert)
import Test.QuickCheck.Property             (Property)
import Control.Monad.Identity (Identity(Identity))

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
main = putStrLn "" >> defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Payment"
        [ testGroup "State"
            [ testProperty "Sender/receiver match" $
              testPaymentSessionM checkSendRecvStateMatch
            , testProperty "Sent amount == received amount" $
              testPaymentSessionM checkRecvSendAmount
            ]
        , testGroup "Settlement tx"
            [ testProperty "Sender value" $
                testPaymentSessionM checkSenderValue
            , testProperty "Receiver value" $
                testPaymentSessionM checkReceiverValue
            , testProperty "At least one output" $
                testPaymentSessionM alwaysOneOutput
            ]
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
                (jsonSerDeser :: ServerPayChanX -> Bool)
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
            (recvPrvKey resInitPair) (SatoshisPerByte 0) KeepDust
    return $ either (error . show) id settleTxE

checkSenderValue :: ChannelPairResult -> TestM Bool
checkSenderValue cpr@ChannelPairResult{..} = do
    settleTx <- mkSettleTx cpr
    let clientChangeOutIndex = indexOf settleTx (fundingAddress resSendChan)
        clientChangeAmount = maybe 0 (HT.outValue . (HT.txOut settleTx !!)) clientChangeOutIndex
    -- Check that the client change amount in the settlement transaction equals the
    --  channel funding amount minus the sum of all payment amounts.
    let fundingVal = fundingValue $ pcsPayment (getChanState resRecvChan)
        fundValMinusPaym = fundingVal - fromIntegral (sum resSentAmounts)
    return $ fromIntegral clientChangeAmount == fundValMinusPaym

checkReceiverValue :: ChannelPairResult -> TestM Bool
checkReceiverValue cpr@ChannelPairResult{..} = do
    settleTx <- mkSettleTx cpr
    let recvOutIndex = indexOf settleTx recvSettleAddr
        recvAmount = maybe 0 (HT.outValue . (HT.txOut settleTx !!)) recvOutIndex
    -- Check receiver amount in settlement transaction with zero fee equals sum
    -- of all payments.
    return $ (fromIntegral recvAmount :: BtcAmount) == fromIntegral (sum resRecvdAmounts)

alwaysOneOutput :: ChannelPairResult -> TestM Bool
alwaysOneOutput cpr = not . null . HT.txOut <$> mkSettleTx cpr

checkSendRecvStateMatch :: ChannelPairResult -> TestM Bool
checkSendRecvStateMatch ChannelPairResult{..} =
    return $ getChanState resSendChan == getChanState resRecvChan

checkRecvSendAmount :: ChannelPairResult -> TestM Bool
checkRecvSendAmount ChannelPairResult{..} =
    return $ if sum resSentAmounts == sum resRecvdAmounts then
        True else error $ show (resSentAmounts, resRecvdAmounts)

checkSpendCondTx :: ChannelPairResult -> TestM Bool
checkSpendCondTx cpr@ChannelPairResult{..} = do
    tx <- mkSettleTx cpr
    let rdmScr = pairRedeemScript $ pcsPayment $ spcState resSendChan
    let ins = getPrevIn tx rdmScr :: [InputG (Pay2 (ScriptHash (Witness (Cond ChanParams)))) ()]
    return $ show ins `trace` True

testPaymentSessionM ::
    (ChannelPairResult -> TestM Bool)
    -> ArbChannelPair
    -> [BtcAmount]
    -> Property
testPaymentSessionM testFunc arbChanPair payLst =
    monadic runTestM $
        run (runChanPair arbChanPair payLst) >>= run . testFunc >>= assert


-- testPaymentSessionM' ::
--     (ChannelPairResult -> TestM Property)
--     -> ArbChannelPair
--     -> Property
-- testPaymentSessionM' testFunc arbChanPair =
--     monadic runTestM $
--         run (runChanPair arbChanPair) >>= run . testFunc


jsonSerDeser :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a) => a -> Bool
jsonSerDeser fp =
    maybe False checkEquals decodedObj
        where decodedObj = JSON.decode $ JSON.encode fp
              checkEquals serDeserVal =
                if serDeserVal /= fp then
                        error $ "Ser/deser mismatch.\nOriginal: " ++
                                show fp ++ "\nCopy: " ++ show decodedObj
                    else
                        True

binSerDeser :: (Typeable a, Show a, Eq a, Bin.Serialize a) => a -> Bool
binSerDeser fp =
    checkEquals decodeRes
        where bs = Bin.encode fp
              decodeRes = deserEither bs
              checkEquals serDeserRes = case serDeserRes of
                    Left e      -> error $ "Serialize/deserialize error: " ++ show e
                    Right res   ->
                            if res /= fp then
                                error $ "Ser/deser mismatch.\nOriginal: " ++ show fp ++
                                        "\nCopy: " ++ show res
                            else
                                True


