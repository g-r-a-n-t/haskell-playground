module Qualities (
  hasClosure,
  isAssociative,
  isCommutative,
  hasIdentity,
  isInvertible,
  isGeneralMap,
  isBijective,
  isSurjective,
  isInjective
) where

import Data.List.Unique

hasClosure :: Eq a => [a] -> (a -> a -> a) -> Bool
hasClosure _S op = and [elem (op a b) _S | (a, b) <- pairs]
                   where pairs = [(a, b) | a <- _S, b <- _S]

isAssociative :: Eq a => [a] -> (a -> a -> a) -> Bool
isAssociative _S op = and [op (op a b) c == op a (op b c) | (a, b, c) <- triplets]
                      where triplets = [(a, b, c) | a <- _S, b <- _S, c <- _S]

isCommutative :: Eq a => [a] -> (a -> a -> a) -> Bool
isCommutative _S op = and [op a b == op b a | (a, b) <- pairs]
                        where pairs = [(a, b) | a <- _S, b <- _S]

hasIdentity :: Eq a => [a] -> a -> (a -> a -> a) -> Bool
hasIdentity _S e op
  | not $ elem e _S = False                               -- e <- A
  | otherwise = all (\a -> op e a == a && op a e == a) _S -- e * a = a * e = a, a <- A

isInvertible :: Eq a => [a] -> a -> (a -> a) -> (a -> a -> a) -> Bool
isInvertible _S e inv op
 | not $ all (\(_, i) -> elem i _S) pairs = False                -- -a <- A, a <- A
 | otherwise = all (\(a, i) -> op a i == e && op i a == e) pairs -- a * i = i * a = e, a <- A
  where pairs = [(a, inv a) | a <- _S]

-- All elements of f(A) map to an element in B.
isGeneralMap :: Eq a => Eq b => (a -> b) -> [a] -> [b] -> Bool
isGeneralMap f _A _B = all (\a -> elem (f a) _B) _A

-- All elements of B are contained in f(A).
isSurjective :: Eq a => Eq b => (a -> b) -> [a] -> [b] -> Bool
isSurjective f _A _B
  | not $ isGeneralMap f _A _B = False
  | otherwise = all (\b -> elem b _A') _B
  where _A' = map f _A

-- All elements of f(A) occur once.
isInjective :: Eq a => Eq b => (a -> b) -> [a] -> [b] -> Bool
isInjective f _A _B
  | not $ isGeneralMap f _A _B = False
  | otherwise = all (\a' -> countElem a' _A' == 1) _A'
  where _A' = map f _A

isBijective :: Eq a => Eq b => (a -> b) -> [a] -> [b] -> Bool
isBijective f _A _B = isInjective f _A _B && isSurjective f _A _B
