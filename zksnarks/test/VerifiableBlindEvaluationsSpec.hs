module VerifiableBlindEvaluationsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import VerifiableBlindEvaluation

-- article: https://electriccoin.co/blog/snark-explain2/
spec :: Spec
spec = do
  describe "Test the examples provided in the fourth section." $ do
    it "Bob should be able verify Alice's alpha pair." $ do
      a' * x `mod` 11 `shouldBe` b'
    it "Bob should be able verify the result of alices polynomial computation." $ do
      _a' * _x `mod` 11 `shouldBe` _b'
