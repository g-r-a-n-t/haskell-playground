module QualitiesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Qualities

first10PositiveIntegers = [1..10]
first10PositiveIntegersAsFloats = [1..10] :: [Float]
integersMod7 = [0..6]

additionMod7 a b = (a + b) `mod` 7
additiveInverseMod7 a = (-a) `mod` 7

spec :: Spec
spec = do
  describe "Qualities.hasClossure" $ do
    it "returns true for integers mod 7 over addition." $ do
      hasClosure integersMod7 additionMod7 `shouldBe` True
    it "returns false for first 10 positive integers over addition." $ do
      hasClosure first10PositiveIntegers (+) `shouldBe` False

  describe "Qualities.isAssociative" $ do
    it "returns true for first 10 positive integers over addition." $ do
      isAssociative first10PositiveIntegers (+) `shouldBe` True
    it "returns false for first 10 positive integers over exponentiation." $ do
      isAssociative first10PositiveIntegers (^) `shouldBe` False
    it "returns true for first 10 positive integers as floats over division." $ do
      isAssociative first10PositiveIntegersAsFloats (/) `shouldBe` False

  describe "Qualities.isCommunicative" $ do
    it "returns true for first 10 positive integers over addition." $ do
      isCommunicative first10PositiveIntegers (+) `shouldBe` True
    it "returns false for first 10 positive integers over exponentiation." $ do
      isCommunicative first10PositiveIntegers (^) `shouldBe` False
    it "returns false for first 10 positive floats over division." $ do
      isCommunicative first10PositiveIntegersAsFloats (/) `shouldBe` False

  describe "Qualities.isInvertible" $ do
    it "returns true for the integers mod 7 over addition." $ do
      isInvertible integersMod7 0 additiveInverseMod7 additionMod7 `shouldBe` True
