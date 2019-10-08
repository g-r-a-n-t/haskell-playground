module Rings (
  Ring(Ring),
  newRing
) where

import Groups
import Properties

-- Rings contain the following elements: a carrier set `[a]`, an additive operation `(a -> a -> a)`,
-- and a multiplicative operation `(a -> a -> a)`.
data Ring a = Ring [a] (a -> a -> a) (a -> a -> a)
newRing _S add mul = Ring _S add mul

-- Verifies whether or not the elements provided form a Ring algebra
isRing :: Eq a => Ring a -> (Bool, String)
isRing (Ring _S add mul)
  | not $ fst (isAbelianGroup (newGroup _S add)) = (False, "Not an abelian group over addition.")
  | not $ isAssociative _S mul = (False, "The multapicative operation is not associative.")
  | not $ isDistributive _S add mul = (False, "The operations are not distributive.")
  | otherwise = (True, "")
