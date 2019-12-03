{-# LANGUAGE DataKinds #-}
module RingsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Modular
import Control.Exception (evaluate)
import Properties

import Rings

integersMod8AddMul = newRing _S (+) (*)
  where _S = [0..7] :: [Mod Integer 8]

spec :: Spec
spec = do
  describe "Rings.isRing" $ do
    it "returns true for integers mod 8." $ do
      isRing integersMod8AddMul `shouldNotBe` (False, "")
