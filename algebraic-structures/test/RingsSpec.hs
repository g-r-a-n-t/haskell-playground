module RingsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Properties



spec :: Spec
spec = do
  describe "Rings.isRing" $ do
    it "isRing" $ do
      True `shouldBe` True
