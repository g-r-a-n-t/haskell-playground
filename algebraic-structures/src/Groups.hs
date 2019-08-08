module Groups (
  Group,
  verify
) where

import Qualities

-- Groups contain the following elements: a carrier set `[a]`, an identity element `a`, an inverse operation `(a -> a)`,
-- and an additive operation `(a -> a -> a)`.
data Group a = Group [a] a (a -> a) (a -> a -> a)

-- Verifies that the Group elements do in fact form a Group algebra.
-- This is only computationally feasible on small carrier sets.
verify :: Ord a => Eq a => Group a -> (Bool, String)
verify (Group _G e inv add)
  | not $ hasClosure _G add = (False, "The group does not have closure over addition.")
  | not $ isAssociative _G add = (False, "Addition is not associative.")
  | not $ hasIdentity _G e add = (False, "The group does not have a valid identity element.")
  | not $ isInvertible _G e inv add = (False, "The group is not invertible.")
  | otherwise = (True, "")


