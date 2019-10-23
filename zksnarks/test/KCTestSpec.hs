module KCTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import KCTest

spec :: Spec
spec = do
  describe "Test an example based on the third article." $ do
    it "Bob can verify Alice's pair." $ do
      x * a' `mod` 11 `shouldBe` b'
