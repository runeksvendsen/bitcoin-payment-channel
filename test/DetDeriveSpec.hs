module DetDeriveSpec where

--import Bitcoin.Internal.Types
import           Bitcoin.BIP32.DetDerive
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C8
import qualified Network.Haskoin.Crypto    as HC
import qualified Network.Haskoin.Test      as HT
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

newtype ArbRootPrv = ArbRootPrv RootPrv
instance Arbitrary ArbRootPrv where
    arbitrary = do
        ArbitraryDerivSeed bs <- arbitrary
        return $ ArbRootPrv $ createRootPrv bs

spec :: Spec
spec = describe "Deterministic key derivation" $
    it "derives same XPubKey for ChildPair & ChildPub" $ do
        ArbRootPrv rootPrv <- generate arbitrary
        arbSeed <- generate arbitrary
        let pub  = derivExtPub (fromRootPrv rootPrv) arbSeed
            pair = derivExtPair rootPrv arbSeed
        (getKey pub :: HC.XPubKey) `shouldBe` getKey pair

derivExtPub
    :: RootPub
    -> ArbitraryDerivSeed
    -> External ChildPub
derivExtPub = detDerive

derivExtPair
    :: RootPrv
    -> ArbitraryDerivSeed
    -> External ChildPair
derivExtPair = detDerive

newtype ArbitraryDerivSeed = ArbitraryDerivSeed BS.ByteString

instance Arbitrary ArbitraryDerivSeed where
    arbitrary = do
        len <- choose (0,32)
        c8Lst <- vector len
        return $ ArbitraryDerivSeed $ C8.pack c8Lst

instance DerivationSeed ArbitraryDerivSeed where
    toDerivSeed (ArbitraryDerivSeed bs) = bs
