import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Qualities

finitePositiveIntegers = [1..10]
finitePositiveFloats = [1..10] :: [Float]
finitePositiveIntegersOver7 = [0..6]
addition a b = a + b
exponentiation a b = a ^ b
division a b = a / b
additionOver7 a b = (a + b) `mod` 7

main :: IO ()
main = hspec $ do
  describe "Qualities.hasClossure" $ do
    it "returns true for integers mod 7 over addition" $ do
      hasClosure finitePositiveIntegersOver7 additionOver7 `shouldBe` True
    it "returns false for finite positive integers over addition" $ do
      hasClosure finitePositiveIntegers addition `shouldBe` False

  describe "Qualities.isAssociative" $ do
    it "returns true for finite positive integers over addition" $ do
      isAssociative finitePositiveIntegers addition `shouldBe` True
    it "returns false for finite positive integers over exponentiation" $ do
      isAssociative finitePositiveIntegers exponentiation `shouldBe` False
    it "returns true for finite positive floats over division" $ do
      isAssociative finitePositiveFloats division `shouldBe` False

  describe "Qualities.isCommunicative" $ do
    it "returns true for finite positive integers over addition" $ do
      isCommunicative finitePositiveIntegers addition `shouldBe` True
    it "returns false for finite positive integers over exponentiation" $ do
      isCommunicative finitePositiveIntegers exponentiation `shouldBe` False
    it "returns false for finite positive floats over division" $ do
      isCommunicative finitePositiveFloats division `shouldBe` False
