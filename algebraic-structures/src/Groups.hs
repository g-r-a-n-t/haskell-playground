module Groups (
  newGroup,
  isGroup,
  isAbelianGroup
) where

import Qualities

-- Groups contain the following elements: a carrier set `[a]`, an identity element `a`, an inverse operation `(a -> a)`,
-- and an operation `(a -> a -> a)`.
data Group a = Group [a] a (a -> a) (a -> a -> a)
newGroup _G e inv op = Group _G e inv op

-- Verifies that the Group elements do in fact form a Group algebra.
-- This is only computationally feasible on small carrier sets.
isGroup :: Eq a => Group a -> (Bool, String)
isGroup (Group _G e inv op)
  | not $ hasClosure _G op = (False, "The group does not have closure.")
  | not $ isAssociative _G op = (False, "The operation is not associative.")
  | not $ hasIdentity _G e op = (False, "The group does not have a valid identity element.")
  | not $ isInvertible _G e inv op = (False, "The group is not invertible.")
  | otherwise = (True, "")

isAbelianGroup :: Eq a => Group a -> (Bool, String)
isAbelianGroup (Group _G e inv op)
  | not $ isGroupRes = (False, isGroupErr)
  | not $ isCommunicative _G op = (False, "The group is not communicative.")
  | otherwise = (True, "")
  where (isGroupRes, isGroupErr) = isGroup (Group _G e inv op)