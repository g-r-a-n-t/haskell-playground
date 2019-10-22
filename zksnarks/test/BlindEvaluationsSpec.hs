module BlindEvaluationsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import BlindEvaluations

-- article: https://electriccoin.co/blog/snark-explain2/
spec :: Spec
spec = do
  describe "Test the example provided in the second section." $ do
    it "The weighted sum computed by Alice should be correct." $ do
      ar `shouldBe` ws'