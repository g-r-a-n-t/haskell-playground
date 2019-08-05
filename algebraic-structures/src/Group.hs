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
verify (Group _G e inv add)
  | not $ elem e _G = (False, "The identity element is not in the carrier set.")
  | not $ map (\c -> add e c) _G == _G = (False, "The identity element is not a valid left operand.")
  | not $ map (\c -> add c e) _G == _G = (False, "The identity element is not a valid right operand.")
  | not $ Set.fromList _Ginv == Set.fromList _G = (False, "The inverted carrier set is not equal to the carrier set.")
  | nub (map (\(i, c) -> add i c) (zip _Ginv _G)) /= [e] = (False, "The inverse of an element is not a valid left operand.")
  | nub (map (\(c, i) -> add c i) (zip _G _Ginv)) /= [e] = (False, "The inverse of an element is not a valid right operand.")
  | not $ isSubset [add a b | (a, b) <- pairs] _G = (False, "There is not closure over the carrier set.")
  | [add (add a b) c | (a, b, c) <- triplets] /= [add a (add b c) | (a, b, c) <- triplets] = (False, "The additive operations in not associative.")
  | otherwise = (True, "")
  where _Ginv    = map inv _G
        pairs    = [(a, b) | a <- _G, b <- _G]
        triplets = [(a, b, c) | a <- _G, b <- _G, c <- _G]

isSubset :: Ord a => Eq a => [a] -> [a] -> Bool
isSubset a b = Set.isSubsetOf (Set.fromList a) (Set.fromList b)