module Properties (
  hasClosure,
  isAssociative,
  isCommutative,
  hasIdentity,
  isInvertible,
  isDistributive,
  isGeneralMap,
  isBijective,
  isSurjective,
  isInjective
) where

import Data.List.Unique
import Data.Maybe

hasClosure :: Eq a => [a] -> (a -> a -> a) -> Bool
hasClosure _S op = and [elem (op a b) _S | (a, b) <- pairs]
                   where pairs = [(a, b) | a <- _S, b <- _S]

isAssociative :: Eq a => [a] -> (a -> a -> a) -> Bool
isAssociative _S op = and [op (op a b) c == op a (op b c) | (a, b, c) <- triplets]
                      where triplets = [(a, b, c) | a <- _S, b <- _S, c <- _S]

isCommutative :: Eq a => [a] -> (a -> a -> a) -> Bool
isCommutative _S op = and [op a b == op b a | (a, b) <- pairs]
                        where pairs = [(a, b) | a <- _S, b <- _S]

identity :: Eq a => [a] -> (a -> a -> a) -> Maybe a
identity _S op
  | length es /= (1 :: Int) = Nothing
  | otherwise = Just (es!!0)
  where es = filter (\e -> all (\a -> op e a == a && op a e == a) _S) _S -- e * a = a * e = a, a <- A

hasIdentity :: Eq a => [a] ->  (a -> a -> a) -> Bool
hasIdentity _S op
  | e == Nothing = False
  | otherwise = True
  where e = identity _S op

isInvertible :: Eq a => [a] -> (a -> a -> a) -> Bool
isInvertible _S op
  | isNothing e = False
  | otherwise = all (\a -> any (\b -> op a b == fromJust e) _S) _S
  where e = identity _S op

isLeftDistributive :: Eq a => [a] -> (a -> a -> a) -> (a -> a -> a) -> Bool
isLeftDistributive _S add mul = all (\(a, b, c) -> mul a (add b c) == add (mul a b) (mul a c)) trips
  where trips = [(a, b, c) | a <- _S, b <- _S, c <- _S]

isRightDistributive :: Eq a => [a] -> (a -> a -> a) -> (a -> a -> a) -> Bool
isRightDistributive _S add mul = all (\(a, b, c) -> mul (add b c) a == add (mul b a) (mul c a)) trips
  where trips = [(a, b, c) | a <- _S, b <- _S, c <- _S]

isDistributive :: Eq a => [a] -> (a -> a -> a) -> (a -> a -> a) -> Bool
isDistributive _S add mul = isLeftDistributive _S add mul && isRightDistributive _S add mul

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
