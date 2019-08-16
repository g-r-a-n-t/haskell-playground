module Groups (
  newGroup,
  newHomomorphism,
  isGroup,
  isAbelianGroup,
  isHomomorphic
) where

import Qualities

-- Groups contain the following elements: a carrier set `[a]`, an identity element `a`, an inverse operation `(a -> a)`,
-- and an operation `(a -> a -> a)`.
data Group a = Group [a] a (a -> a) (a -> a -> a)
newGroup _S e inv op = Group _S e inv op

-- Group homomorphisms consist of the following elements: two groups `Group a` and `Group b` and a map from the first
-- set to the second `(a -> b)`.
data Homomorphism a b = Homomorphism (a -> b) (Group a) (Group b)
newHomomorphism f _G _H = Homomorphism f _G _H

-- Group isomorphisms consist of the following elements: two groups `Group a` and `Group b` and a map from the first
-- set to the second `(a -> b)`.
data Isomorphism a b = Isomorphism (a -> b) (Group a) (Group b)
newIsomorphism f _G _H = Isomorphism f _G _H

-- Verifies that the elements do in fact form a Group algebra.
-- This is only computationally feasible on small carrier sets.
isGroup :: Eq a => Group a -> (Bool, String)
isGroup (Group _S e inv op)
  | not $ hasClosure _S op = (False, "The group does not have closure.")
  | not $ isAssociative _S op = (False, "The operation is not associative.")
  | not $ hasIdentity _S e op = (False, "The group does not have a valid identity element.")
  | not $ isInvertible _S e inv op = (False, "The group is not invertible.")
  | otherwise = (True, "")

-- Verifies that the elements do in fact form an Abelian Group algebra.
isAbelianGroup :: Eq a => Group a -> (Bool, String)
isAbelianGroup (Group _S e inv op)
  | not $ isGroupRes = (False, isGroupErr)
  | not $ isCommutative _S op = (False, "The group is not commutative.")
  | otherwise = (True, "")
  where (isGroupRes, isGroupErr) = isGroup (Group _S e inv op)

-- Verifies that the elements do in fact form a Homomorphism.
-- This assumes each of the groups provided are valid.
isHomomorphic :: Eq a => Eq b => Homomorphism a b -> (Bool, String)
isHomomorphic (Homomorphism f _G _H)
  | not $ isGeneralMap f _Sg _Sh = (False, "Not a general map.")
  | not $ all (\(a, b) -> f (opG a b) == opH (f a) (f b)) pairs = (False, "The equality check failed.")
  | otherwise = (True, "")
  where pairs = [(a, b) | a <- _Sg, b <- _Sg]
        Group _Sg _ _ opG = _G
        Group _Sh _ _ opH = _H

isIsomorphic :: Eq a => Eq b => Isomorphism a b -> (Bool, String)
isIsomorphic (Isomorphism f _G _H)
  | not $ isHomoRes = (False, isHomoErr)
  | not $ isBijective f _Sg _Sh = (False, "The mapping function is not bijective.")
  | otherwise = (True, "")
  where (isHomoRes, isHomoErr) = isHomomorphic (newHomomorphism f _G _H)
        Group _Sg _ _ _ = _G
        Group _Sh _ _ _ = _H
