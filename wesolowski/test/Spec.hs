import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Wesolowski
import Crypto.Random.DRBG

main :: IO ()
main = hspec $ do
  describe "Wesolowski.trapdoor" $ do
    it "solves the VDF" $ do
      trapdoor 143 120 42 99 `shouldBe` (113, 0)

  describe "Wesolowski.eval" $ do
    it "solves the VDF" $ do
      eval 143 42 99 `shouldBe` (113, 0)

  describe "Wesolowski.keygen" $ do
    it "creates a valid keypair" $ do
      g <- newGenIO :: IO CtrDRBG
      let Right keypair = keygen g 10
      keypair `shouldSatisfy` (\(pk,sk) -> pk > sk)
