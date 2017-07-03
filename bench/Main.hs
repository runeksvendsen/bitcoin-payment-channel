{-# LANGUAGE CPP, BangPatterns #-}
module Main where

import qualified PaymentChannel.Test as Pay
import Criterion.Main
import Test.QuickCheck  (sample')

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData B.ByteString
#endif

payCount = 1000 :: Integer

main :: IO ()
main = do
  (arbPair,_)   <- fmap head $ sample' $ Pay.mkChanPairInitAmount 0
  let mkCappedPayment :: Pay.BtcAmount -> (Pay.ClientPayChan, Pay.SignedPayment, Pay.BtcAmount)
      mkCappedPayment  = Pay.createPaymentCapped (Pay.sendChan arbPair) . Pay.Capped
      (_,!payment,_) = mkCappedPayment 1
  let multiMkVerify = Pay.runChanPair arbPair (fromIntegral <$> [1..payCount])
  defaultMain
    [ bench "Create payment" $ nf mkCappedPayment 1
    , bench "Verify payment" $ nfIO
        ( either (error . show) id <$> Pay.acceptPayment (Pay.toPaymentData payment) (Pay.recvChan arbPair) )
    , bench (show payCount ++ " rounds of create+verify payment") $ nfIO multiMkVerify
    ]
