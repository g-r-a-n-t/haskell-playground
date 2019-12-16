module BLSSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import BLS

spec = do
  describe "BLS" $ do
    it "true is true" $ do
      True `shouldBe` True