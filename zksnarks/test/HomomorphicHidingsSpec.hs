module HomomorphicHidingsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HomomorphicHidings

spec :: Spec
spec = do
  describe "HH.true should be true" $ do
    it "a test" $ do
      true `shouldBe` True