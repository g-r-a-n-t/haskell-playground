module Groups (
  newGroup,
  verify
) where

import Qualities

-- Groups contain the following elements: a carrier set `[a]`, an identity element `a`, an inverse operation `(a -> a)`,
-- and an operation `(a -> a -> a)`.
data Group a = Group [a] a (a -> a) (a -> a -> a)

newGroup _G e inv op = Group _G e inv op

-- Verifies that the Group elements do in fact form a Group algebra.
-- This is only computationally feasible on small carrier sets.
verify :: Ord a => Eq a => Group a -> (Bool, String)
verify (Group _G e inv op)
  | not $ hasClosure _G op = (False, "The group does not have closure over opition.")
  | not $ isAssociative _G op = (False, "Addition is not associative.")
  | not $ hasIdentity _G e op = (False, "The group does not have a valid identity element.")
  | not $ isInvertible _G e inv op = (False, "The group is not invertible.")
  | otherwise = (True, "")


