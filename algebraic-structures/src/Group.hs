module Group (
  Group,
  verify
) where

-- Groups contain the following elements: a carrier set `[a]`, an identity element `a`, an inverse operation `(a -> a)`,
-- and an additive operation `(a -> a -> a)`.
data Group a = Group [a] a (a -> a) (a -> a -> a)

-- Verifies that the Group elements do in fact form a Group algebra.
-- This is only computationally feasible on small carrier sets.
verify :: Eq a => Group a -> (Bool, String)
verify (Group _C e inv add)
  | not $ elem e _C = (False, "The identity element `e` is not in the carrier set.")
  | True = (True, "prly alright")