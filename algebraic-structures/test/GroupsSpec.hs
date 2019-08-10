{-# LANGUAGE DataKinds #-}
module GroupsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Groups
import Data.Modular


multiplicativeGroupOfIntegersModulo9 = newGroup _G e inv' mul
  where _G  = [1,2,4,5,7,8] :: [Mod Integer 9]
        e   = 1 :: Mod Integer 9
        inv' a = inv a :: Mod Integer 9
        mul a b = a * b :: Mod Integer 9

first10IntegersOverAddition = newGroup _G e inv add
 where _G = [1..10]
       e  = 0
       inv a = -a
       add a b = a + b

spec :: Spec
spec = do
  describe "Groups.isGroup" $ do
    it "returns true for the multiplicative group of integers modulo 9." $ do
      isGroup multiplicativeGroupOfIntegersModulo9 `shouldBe` (True, "")
    it "returns false for the first 10 positive integers over addition." $ do
      isGroup first10IntegersOverAddition `shouldNotBe` (True, "")

  describe "Groups.isAbelianGroup" $ do
    it "returns true for the multiplicative group of integers modulo 9." $ do
      isAbelianGroup multiplicativeGroupOfIntegersModulo9 `shouldBe` (True, "")
