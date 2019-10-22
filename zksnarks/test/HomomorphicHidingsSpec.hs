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
      isHomomorphic zpToZxpByGx `shouldNotBe` (True, "")

-- This test fails (when shouldNotBe is replaced with shouldBe), kinda pointless to check because the HH discussed
-- in the section isn't a group homomorphism for a couple of reasons.
-- a.) Z11 where the group contains {0,1,...,9} is not a valid group since 1 + 9 = 10 and 10 is not in the group.
-- b.) The mapping function from Z11 to Zx11 (f(x) = g^x) does not actually hold equality if x is a sum greater than 11.
