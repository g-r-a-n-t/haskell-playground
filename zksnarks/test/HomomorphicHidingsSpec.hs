module HomomorphicHidingsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HomomorphicHidings
import Groups

-- article: https://electriccoin.co/blog/snark-explain/
spec :: Spec
spec = do
  describe "Test the realistic HH example in the first article." $ do
    it "The HH is a homomorphism" $ do
      isHomomorphic zpToZxpByGx `shouldBe` (True, "")
