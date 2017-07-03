{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BitcoinSpec where

import           Bitcoin.Signature
import           Data.Monoid            ((<>))
import           Network.Haskoin.Test
import           PaymentChannel.Test
import           Test.Hspec
import           Test.QuickCheck
-- HELLO
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Test   as HC


newtype ArbitrarySpendCond = ArbitrarySpendCond ArbitraryScript
    deriving (Eq, Show, Arbitrary)

newtype ArbitrarySpendFulfill = ArbitrarySpendFulfill ArbitraryScript
    deriving (Eq, Show, Arbitrary)


instance SpendCondition ArbitrarySpendCond where
    conditionScript (ArbitrarySpendCond (ArbitraryScript s)) = s

instance SpendFulfillment ArbitrarySpendFulfill ArbitrarySpendCond where
    signatureScript (ArbitrarySpendFulfill (ArbitraryScript ss)) sc =
        ss <> conditionScript sc
    rawSigs = undefined


main :: IO ()
main = hspec spec

spec :: Spec
spec =
    keySpec

keySpec :: Spec
keySpec  = do
  describe "External ChildPub/ChildPair" $
    it "derive same pubkeys & created signatures verify" $ do
      rootKey  <- generate arbitrary
      HC.ArbitrarySoftPath derivPath <- generate arbitrary
      let pair :: External ChildPair
          pair = mkChild (rootKey :: RootPrv) derivPath
          pub :: External ChildPub
          pub  = mkChild (fromRootPrv rootKey) derivPath
      (getKey pair :: HC.XPubKey) `shouldBe` (getKey pub :: HC.XPubKey)
      HC.ArbitraryHash256 h256 <- generate arbitrary
      -- External pair/pub compare
      HC.signMsg h256 (getKey pair :: HC.PrvKeyC) `shouldSatisfy`
          (\sig -> HC.verifySig h256 sig (getKey pub :: HC.PubKeyC))
      -- External pair/pair compare
      HC.signMsg h256 (getKey pair :: HC.PrvKeyC) `shouldSatisfy`
          (\sig -> HC.verifySig h256 sig (getKey pair :: HC.PubKeyC))
  describe "Internal ChildPair" $
    it "created signatures verify" $ do
      rootKey  <- generate arbitrary
      HC.ArbitraryHardPath derivPath <- generate arbitrary
      let pair :: Internal ChildPair
          pair = mkChild (rootKey :: RootPrv) derivPath
      HC.ArbitraryHash256 h256 <- generate arbitrary
      -- Internal pair/pair compare
      HC.signMsg h256 (getKey pair :: HC.PrvKeyC) `shouldSatisfy`
          (\sig -> HC.verifySig h256 sig (getKey pair :: HC.PubKeyC))


--btcSpec :: Spec
--btcSpec =
--  describe "Bitcoin" $
--    it "signed transaction verifies" $ undefined


--serTest :: (Arbitrary a, Bin.Serialize a)
--        => Gen (Either String a)
--serTest = Bin.decode . Bin.encode <$> arbitrary
