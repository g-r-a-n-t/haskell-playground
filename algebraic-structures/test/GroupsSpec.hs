{-# LANGUAGE DataKinds #-}
module GroupsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Groups
import Data.Modular

multiplicativeGroupOfIntegersMod9 = newGroup _S e inv' (*)
  where _S      = [1,2,4,5,7,8] :: [Mod Integer 9]
        e       = 1 :: Mod Integer 9
        inv' a  = inv a :: Mod Integer 9

first10PositiveIntegersOverAddition = newGroup _S e inv (+)
 where _S      = [1..10]
       e       = 0
       inv a   = -a

integersMod3OverAddition = newGroup _S e inv' (+)
  where _S     = [0..2] :: [Mod Integer 3]
        e      = 0 :: Mod Integer 3
        inv' a = -a :: Mod Integer 3

integersMod6OverAddition = newGroup _S e inv' (+)
  where _S     = [0..5] :: [Mod Integer 6]
        e      = 0 :: Mod Integer 6
        inv' a = -a :: Mod Integer 6

z3ToZ6by2x = newHomomorphism f integersMod3OverAddition integersMod6OverAddition
  where f x = 2 * toMod'(x) :: Mod Integer 6

z3ToZN9by2x = newHomomorphism f integersMod3OverAddition multiplicativeGroupOfIntegersMod9
  where f x = 2 * toMod'(x) :: Mod Integer 9

spec :: Spec
spec = do
  describe "Groups.isGroup" $ do
    it "returns true for the multiplicative group of integers mod 9." $ do
      isGroup multiplicativeGroupOfIntegersMod9 `shouldBe` (True, "")
    it "returns false for the first 10 positive integers over addition." $ do
      isGroup first10PositiveIntegersOverAddition `shouldNotBe` (True, "")

  describe "Groups.isAbelianGroup" $ do
    it "returns true for the multiplicative group of integers modulo 9." $ do
      isAbelianGroup multiplicativeGroupOfIntegersMod9 `shouldBe` (True, "")
    it "returns true for the integers mod 3 over addition." $ do
      isAbelianGroup integersMod3OverAddition `shouldBe` (True, "")

  describe "Groups.isHomomorphic" $ do
    it "returns true for the integers mod 3 to integers mod 6 by f = 2x." $ do
      isHomomorphic z3ToZ6by2x `shouldBe` (True, "")
    it "returns false for the integers mod 3 to multiplicative group of integer mod 9 by f = 2x." $ do
      isHomomorphic z3ToZN9by2x `shouldNotBe` (True, "")