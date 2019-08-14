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

--z3ToZ6by2x =

spec :: Spec
spec = do
  describe "Groups.isGroup" $ do
    it "returns true for the multiplicative group of integers modulo 9." $ do
      isGroup multiplicativeGroupOfIntegersMod9 `shouldBe` (True, "")
    it "returns false for the first 10 positive integers over addition." $ do
      isGroup first10PositiveIntegersOverAddition `shouldNotBe` (True, "")

  describe "Groups.isAbelianGroup" $ do
    it "returns true for the multiplicative group of integers modulo 9." $ do
      isAbelianGroup multiplicativeGroupOfIntegersMod9 `shouldBe` (True, "")
    it "returns true for the integers mod 3 over addition." $ do
      isAbelianGroup integersMod3OverAddition `shouldBe` (True, "")
