module Group (
  Group,
  verify
) where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- Groups contain the following elements: a carrier set `[a]`, an identity element `a`, an inverse operation `(a -> a)`,
-- and an additive operation `(a -> a -> a)`.
data Group a = Group [a] a (a -> a) (a -> a -> a)

-- Verifies that the Group elements do in fact form a Group algebra.
-- This is only computationally feasible on small carrier sets.
verify :: Ord a => Eq a => Group a -> (Bool, String)
verify (Group _C e inv add)
  | not $ elem e _C = (False, "The identity element is not in the carrier set.")
  | not $ map (\c -> add e c) _C == _C = (False, "The identity element is not a valid left operand.")
  | not $ map (\c -> add c e) _C == _C = (False, "The identity element is not a valid right operand.")
  | not $ Set.fromList _Cinv == Set.fromList _C = (False, "The inverted carrier set is not equal to the carrier set.")
  | nub (map (\(i, c) -> add i c) (zip _Cinv _C)) /= [e] = (False, "The inverse of an element is not a valid left operand.")
  | nub (map (\(c, i) -> add c i) (zip _C _Cinv)) /= [e] = (False, "The inverse of an element is not a valid right operand.")
  | otherwise = (True, "")
  where _Cinv    = map inv _C
        pairs    = [(i, j) | i <- _C, j <- _C]
        triplets = [(i, j, k) | i <- _C, j <- _C, k <- _C]
