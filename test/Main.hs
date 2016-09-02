{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bitcoin.PaymentChannel
import Data.Bitcoin.PaymentChannel.Types
import Data.Bitcoin.PaymentChannel.Util
import qualified Data.Bitcoin.PaymentChannel.Internal.State as S

import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Crypto as HC
-- import Network.Haskoin.Test.Crypto
import Network.Haskoin.Test -- (ArbitraryHash256, ArbitraryTxHash(..))
import Network.Haskoin.Constants
import Data.Maybe
import Control.Monad

import Test.QuickCheck -- (elements, Gen, arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic -- (assert, monadicIO, pick, pre, run)
import Debug.Trace

dUST_LIMIT = 700 :: BitcoinAmount
mIN_CHANNEL_SIZE = 1400 :: BitcoinAmount

testAddrTestnet = "2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469" :: HC.Address
testAddrLivenet = "14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63" :: HC.Address


data ArbChannelPair = ArbChannelPair
    SenderPaymentChannel ReceiverPaymentChannel [BitcoinAmount] (HC.Hash256 -> HC.Signature)

instance Show ArbChannelPair where
    show (ArbChannelPair spc rpc _ _) =
        "SendState: " ++ show spc ++ "\n" ++
        "RecvState: " ++ show rpc

doPayment :: ArbChannelPair -> BitcoinAmount -> ArbChannelPair
doPayment (ArbChannelPair spc rpc payList f) amount =
    let
        (amountSent, pmn, newSpc) = sendPayment spc amount
        eitherRpc = recvPayment rpc pmn
    in
        case eitherRpc of
            Left e -> error (show e)
            Right (rAmt, newRpc) -> ArbChannelPair newSpc newRpc (amountSent : payList) f

instance Arbitrary ArbChannelPair where
    arbitrary = mkChanPair

mkChanPair :: Gen ArbChannelPair -- (Either String ArbChannelPair)
mkChanPair = do
        -- sender key pair
        ArbitraryPubKey sendPriv sendPK <- arbitrary
        -- receiver key pair
        ArbitraryPubKey recvPriv recvPK <- arbitrary
        -- expiration date
        lockTime <- arbitrary
        fti <- arbitrary
        let cp = CChannelParameters
                (MkSendPubKey sendPK) (MkRecvPubKey recvPK) lockTime dUST_LIMIT
        -- total channel value
        let chanAmount = fromIntegral $ ftiOutValue fti :: Integer
        -- value of first payment
        initPayAmount <- arbitrary -- fromIntegral <$> choose (0, chanAmount)

        let (initPayActualAmount,paymnt,sendChan) = channelWithInitialPaymentOf
                cp fti (flip HC.signMsg sendPriv) (HC.pubKeyAddr sendPK) initPayAmount
        let eitherRecvChan = channelFromInitialPayment
                cp fti (HC.pubKeyAddr sendPK) paymnt

        case eitherRecvChan of
            Left e -> error (show e)
            Right (val,recvChan) -> return $ ArbChannelPair
                    sendChan recvChan [initPayActualAmount] (flip HC.signMsg recvPriv)

instance Arbitrary BitcoinLockTime where
    arbitrary = fmap parseBitcoinLocktime arbitrary

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

runChanPair :: ArbChannelPair -> [BitcoinAmount] -> (ArbChannelPair, [BitcoinAmount])
runChanPair chanPair paymentAmountList =
    (foldl doPayment chanPair paymentAmountList, paymentAmountList)

checkChanPair :: (ArbChannelPair, [BitcoinAmount]) -> Bool
checkChanPair (ArbChannelPair sendChan recvChan amountList recvSignFunc, _) = do
    let settleTx = getSettlementBitcoinTx recvChan recvSignFunc testAddrLivenet 0
    let clientChangeAmount = case settleTx of
            (HT.Tx _ _ (clientOut:_) _) ->
                 HT.outValue clientOut
            _ -> error "BUG: Bad settlement tx format"
    -- Check that the client change amount in the settlement transaction equals the
    --  channel funding amount minus the sum of all payment amounts.
    let fundAmountMinusPaySum = S.pcsChannelTotalValue (getChannelState recvChan) -
            fromIntegral (sum amountList)
    let clientValueIsGood = fromIntegral clientChangeAmount == fundAmountMinusPaySum :: Bool
    -- Debug print
    let checkGoodClientValue goodCV = if not goodCV then
                error $ "Bad client change value. Actual: " ++ show clientChangeAmount ++
                        " should be " ++ show fundAmountMinusPaySum
            else
                goodCV
    -- Check that sender/receiver (client/server) agree on channel state
    let statesMatch = getChannelState recvChan == getChannelState sendChan
    checkGoodClientValue clientValueIsGood && statesMatch


testChanPair :: ArbChannelPair -> [BitcoinAmount] -> Bool
testChanPair arbChanPair paymentAmountList =
    checkChanPair (runChanPair arbChanPair paymentAmountList)

main :: IO ()
main = quickCheckWith stdArgs testChanPair




