{-# LANGUAGE DataKinds #-}
module GroupsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Groups
import Data.Modular

inv9 :: Mod Integer 9 -> Mod Integer 9
inv9 a = inv a :: Mod Integer 9

multiplicativeGroupOfIntegersModulo9 = newGroup _G e inv9 mul
  where _G  = [1,2,4,5,7,8] :: [Mod Integer 9]
        e   = 1 :: Mod Integer 9
        mul a b = a * b :: Mod Integer 9

spec :: Spec
spec = do
  describe "Groups.verify" $ do
    it "returns true for the multiplicative group of integers modulo 9." $ do
      Groups.verify multiplicativeGroupOfIntegersModulo9 `shouldBe` (True, "")


