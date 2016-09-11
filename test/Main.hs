module Main where

import Data.Bitcoin.PaymentChannel
import Data.Bitcoin.PaymentChannel.Types
import Data.Bitcoin.PaymentChannel.Util
-- import Data.Bitcoin.PaymentChannel.Internal.Util

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


data ArbChannelPair = ArbChannelPair SenderPaymentChannel ReceiverPaymentChannel
    deriving (Show)

doPayment :: ArbChannelPair -> BitcoinAmount -> ArbChannelPair
doPayment (ArbChannelPair spc rpc) amount =
    let
        (pmn, newSpc) = sendPayment spc amount
        eitherRpc = recvPayment rpc pmn
    in
        case eitherRpc of
            Left e -> error (show e)
            Right (rAmt, newRpc) -> ArbChannelPair newSpc newRpc

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
        let cp = CChannelParameters (MkSendPubKey sendPK) (MkRecvPubKey recvPK) lockTime

        fti <- arbitrary

        -- total channel value
        let chanAmount = fromIntegral $ ftiOutValue fti :: Integer
        -- value of first payment
        initPayAmount <- arbitrary -- fromIntegral <$> choose (0, chanAmount)

        let (paymnt,sendChan) = channelWithInitialPaymentOf
                cp fti (flip HC.signMsg sendPriv) (HC.pubKeyAddr sendPK) initPayAmount
        let eitherRecvChan = channelFromInitialPayment
                cp fti (HC.pubKeyAddr sendPK) paymnt

        case eitherRecvChan of
            Left e -> error (show e) --return $ Left (show e)
            Right (val,recvChan) -> return $ ArbChannelPair sendChan recvChan

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
    arbitrary = fmap fromIntegral (choose (0,round $ 21e6 * 1e8 :: Integer))

runChanPair :: ArbChannelPair -> [BitcoinAmount] -> ArbChannelPair
runChanPair chanPair l = foldl doPayment chanPair l

checkChanPair :: ArbChannelPair -> Bool
checkChanPair (ArbChannelPair sendChan recvChan) =
    getChannelState recvChan == getChannelState sendChan

testChanPair :: ArbChannelPair -> [BitcoinAmount] -> Bool
testChanPair p al = checkChanPair (runChanPair p al)

main :: IO ()
main = do
    quickCheckWith stdArgs -- { maxSuccess = 25 }
        testChanPair
--         (verbose $ testChanPair)



