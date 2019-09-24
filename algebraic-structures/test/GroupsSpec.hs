{-# LANGUAGE DataKinds #-}
module GroupsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Groups
import Data.Modular
import Math.Algebra.Group.PermutationGroup

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

integersMod8OverAddition = newGroup _S e inv' (+)
  where _S     = [0..7] :: [Mod Integer 8]
        e      = 0 :: Mod Integer 8
        inv' a = -a :: Mod Integer 8

integersMod113OverAddition = newGroup _S e inv' (+)
  where _S     = [0..112] :: [Mod Integer 113]
        e      = 0 :: Mod Integer 113
        inv' a = -a :: Mod Integer 113

first5IntegersMod8OverAddition = newGroup _S e inv' (+)
  where _S     = [0..4] :: [Mod Integer 8]
        e      = 0 :: Mod Integer 8
        inv' a = -a :: Mod Integer 8

-- read: https://en.wikipedia.org/wiki/Subgroup#Example:_Subgroups_of_Z8
z8Subgroups = [trivial, _J, _H, _G]
  where (Group _S e inv op) = integersMod8OverAddition
        trivial = newGroup [e] e inv op
        _J      = newGroup [_S!!0, _S!!4] e inv op
        _H      = newGroup [_S!!0, _S!!2, _S!!4, _S!!6] e inv op
        _G      = integersMod8OverAddition

permuFourGroup = newGroup _S e inv (*)
  where _S    = [e, a, b, a * b]
        a     = p [[1,2],[3],[4]]
        b     = p [[1],[2],[3,4]]
        e     = 1
        inv p = p

kleinFourGroup = newGroup _S "e" inv op
  where _S = ["e", "a", "b", "ab"]
        inv a = a
        op a b
          | a == "e" = b -- Product of an element an identity
          | b == "e" = a
          | a == b = "e" -- Inverse of an element is itself
          | a == "a" && b == "b" = "ab" -- Product of "a" and "b" is "ab"
          | a == "b" && b == "a" = "ab"
          | a == "a" || b == "a" = "b" -- At this point we know one of the elements is "ab"
          | a == "b" || b == "b" = "a"

z3ToZ6by2xHomo = newHomomorphism f integersMod3OverAddition integersMod6OverAddition
  where f x = 2 * toMod'(x) :: Mod Integer 6

z3ToZN9by2xHomo = newHomomorphism f integersMod3OverAddition multiplicativeGroupOfIntegersMod9
  where f x = 2 * toMod'(x) :: Mod Integer 9

z3ToZ6by2xIso = newIsomorphism f integersMod3OverAddition integersMod6OverAddition
  where f x = 2 * toMod'(x) :: Mod Integer 6

k4ToM4byMap = newIsomorphism f kleinFourGroup permuFourGroup
  where f x
          | x == "e" = 1
          | x == "a" = p [[1,2],[3],[4]]
          | x == "b" = p [[1],[2],[3,4]]
          | x == "ab" = (f "a") * (f "b")

spec :: Spec
spec = do
  describe "Groups.isGroup" $ do
    it "returns true for the multiplicative group of integers mod 9." $ do
      isGroup multiplicativeGroupOfIntegersMod9 `shouldBe` (True, "")
    it "returns false for the first 10 positive integers over addition." $ do
      isGroup first10PositiveIntegersOverAddition `shouldNotBe` (True, "")
    it "returns true for the permutation four-group." $ do
      isGroup permuFourGroup `shouldBe` (True, "")
    it "returns true for the klein four-group." $ do
      isGroup kleinFourGroup `shouldBe` (True, "")

  describe "Groups.isAbelianGroup" $ do
    it "returns true for the multiplicative group of integers modulo 9." $ do
      isAbelianGroup multiplicativeGroupOfIntegersMod9 `shouldBe` (True, "")
    it "returns true for the integers mod 3 over addition." $ do
      isAbelianGroup integersMod3OverAddition `shouldBe` (True, "")
    it "returns true for the integers mod 113 over addition." $ do
      isAbelianGroup integersMod113OverAddition `shouldBe` (True, "")
    it "returns true for the permutation four-group." $ do
      isAbelianGroup permuFourGroup `shouldBe` (True, "")
    it "returns true for the klein four-group." $ do
      isAbelianGroup kleinFourGroup `shouldBe` (True, "")

  describe "Groups.isHomomorphic" $ do
    it "returns true for the integers mod 3 to integers mod 6 by f = 2x." $ do
      isHomomorphic z3ToZ6by2xHomo `shouldBe` (True, "")
    it "returns false for the integers mod 3 to multiplicative group of integer mod 9 by f = 2x." $ do
      isHomomorphic z3ToZN9by2xHomo `shouldNotBe` (True, "")

  describe "Groups.isIsomorphic" $ do
    it "returns false for the integers mod 3 to integers mod 6 by f = 2x." $ do
      isIsomorphic z3ToZ6by2xIso `shouldNotBe` (True, "")
    it "returns true for the klein four group to the permutation four group by a direct map." $ do
      isIsomorphic k4ToM4byMap `shouldBe` (True, "")

  describe "Groups.isSubgroup" $ do
    it "returns true from the trivial subgroup of Z8." $ do
      isSubgroup (z8Subgroups!!0) integersMod8OverAddition `shouldBe` (True, "")
    it "returns true from the first proper subgroup of Z8, {0, 4}." $ do
      isSubgroup (z8Subgroups!!1) integersMod8OverAddition `shouldBe` (True, "")
    it "returns true from the second proper subgroup of Z8, {0, 2, 4, 6}." $ do
      isSubgroup (z8Subgroups!!2) integersMod8OverAddition `shouldBe` (True, "")
    it "returns true for Z8 itself." $ do
      isSubgroup integersMod8OverAddition integersMod8OverAddition `shouldBe` (True, "")
    it "returns false for an invalid subgroup of Z8, {0,1,2,3,4}." $ do
      isSubgroup first5IntegersMod8OverAddition integersMod8OverAddition `shouldNotBe` (True, "")

  describe "Groups.subgroups" $ do
    it "returns 4 subgroups for Z8" $ do
      length(subgroups integersMod8OverAddition) `shouldBe` 4

  describe "Groups.isSimple" $ do
    it "returns false for Z8" $ do
      Groups.isSimple integersMod8OverAddition `shouldBe` False
    it "returns false for (Z/9Z)*" $ do
      Groups.isSimple multiplicativeGroupOfIntegersMod9 `shouldBe` False
    it "returns true for Z113" $ do
      Groups.isSimple integersMod113OverAddition `shouldBe` True