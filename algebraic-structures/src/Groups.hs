module Groups (
  Group(Group),
  Homomorphism(Homomorphism),
  Isomorphism(Isomorphism),
  newGroup,
  newHomomorphism,
  newIsomorphism,
  isGroup,
  isAbelianGroup,
  isHomomorphic,
  isIsomorphic,
  isSubgroup,
  subgroups,
  isSimple
) where

import Properties

-- Groups contain the following elements: a carrier set `[a]` and an operation `(a -> a -> a)`.
data Group a = Group [a] (a -> a -> a)
newGroup _S op = Group _S op

-- Group homomorphisms consist of the following elements: two groups `Group a` and `Group b` and a map from the first
-- set to the second `(a -> b)`.
data Homomorphism a b = Homomorphism (a -> b) (Group a) (Group b)
newHomomorphism f _G _H = Homomorphism f _G _H

-- Group isomorphisms consist of the following elements: two groups `Group a` and `Group b` and a map from the first
-- set to the second `(a -> b)`.
data Isomorphism a b = Isomorphism (a -> b) (Group a) (Group b)
newIsomorphism f _G _H = Isomorphism f _G _H

-- Verifies that the elements do in fact form a Group algebra.
isGroup :: Eq a => Group a -> (Bool, String)
isGroup (Group _S op)
  | not $ hasClosure _S op = (False, "The operation does not have closure.")
  | not $ isAssociative _S op = (False, "The operation is not associative.")
  | not $ hasIdentity _S op = (False, "The set does not have an identity element.")
  | not $ isInvertible _S op = (False, "The operation is not invertible.")
  | otherwise = (True, "")

-- Verifies that the elements do in fact form an Abelian Group algebra.
isAbelianGroup :: Eq a => Group a -> (Bool, String)
isAbelianGroup (Group _S op)
  | not $ isGroupRes = (False, isGroupErr)
  | not $ isCommutative _S op = (False, "The operation is not commutative.")
  | otherwise = (True, "")
  where (isGroupRes, isGroupErr) = isGroup (Group _S op)

-- Generate a list of subgroups [H] from a group G
-- This assumes that G is a valid group
subgroups :: Eq a => Group a -> [Group a]
subgroups _G = _G:(filter (\_H -> fst (isSubgroup _H _G)) _Hs)
  where (Group _Sg op) = _G
        ords = divisors (length _Sg) -- candidate orders
        _Shs = foldl (\acc ord -> (choose _Sg ord) ++ acc) [] ords -- candidate sets
        _Hs = map (\_Sh' -> (Group _Sh' op)) _Shs -- create groups from sets

-- Verifies that the group H is in fact a subgroup f G
-- This assumes G is a valid group and that operations are consistent between H and G
isSubgroup :: Eq a => Group a -> Group a -> (Bool, String)
isSubgroup _H _G
  | not $ all (\h -> elem h _Sg) _Sh = (False, "An element of H is not in G.")
  | not $ hasClosure _Sh op = (False, "H does not have closure over the provided operation.")
  | not $ isInvertible _Sh op = (False, "H does not have closure over inverses.")
  | otherwise = (True, "")
  where (Group _Sh op) = _H
        (Group _Sg _) = _G


isSimple :: Eq a => Group a -> Bool
isSimple _G = length(subgroups _G) == 2

-- Verifies that the elements do in fact form a Homomorphism.
-- This assumes each of the groups provided are valid.
isHomomorphic :: Eq a => Eq b => Homomorphism a b -> (Bool, String)
isHomomorphic (Homomorphism f _G _H)
  | not $ isGeneralMap f _Sg _Sh = (False, "Not a general map.")
  | not $ all (\(a, b) -> f (opG a b) == opH (f a) (f b)) pairs = (False, "The equality check failed.")
  | otherwise = (True, "")
  where pairs = [(a, b) | a <- _Sg, b <- _Sg]
        Group _Sg opG = _G
        Group _Sh opH = _H

-- Verifies that the elements do in fact form a Homomorphism.
-- This assumes each of the groups provided are valid.
isIsomorphic :: Eq a => Eq b => Isomorphism a b -> (Bool, String)
isIsomorphic (Isomorphism f _G _H)
  | not $ isHomoRes = (False, isHomoErr)
  | not $ isBijective f _Sg _Sh = (False, "The mapping function is not bijective.")
  | otherwise = (True, "")
  where (isHomoRes, isHomoErr) = isHomomorphic (newHomomorphism f _G _H)
        Group _Sg _ = _G
        Group _Sh _ = _H

divisors :: Int -> [Int]
divisors x = filter (\y -> x `mod` y == 0) [1..x `div` 2]

-- Copied from https://stackoverflow.com/questions/14267196/fast-obtention-of-all-the-subsets-of-size-n-in-haskell
choose :: [b] -> Int -> [[b]]
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k