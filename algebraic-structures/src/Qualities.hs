module Qualities (
  hasClosure,
  isAssociative,
  isCommunicative,
  hasIdentity,
  isInvertible
) where

hasClosure :: Eq a => [a] -> (a -> a -> a) -> Bool
hasClosure _A op = and [elem (op a b) _A | (a, b) <- pairs]
                   where pairs = [(a, b) | a <- _A, b <- _A]

isAssociative :: Eq a => [a] -> (a -> a -> a) -> Bool
isAssociative _A op = and [op (op a b) c == op a (op b c) | (a, b, c) <- triplets]
                      where triplets = [(a, b, c) | a <- _A, b <- _A, c <- _A]

isCommunicative :: Eq a => [a] -> (a -> a -> a) -> Bool
isCommunicative _A op = and [op a b == op b a | (a, b) <- pairs]
                        where pairs = [(a, b) | a <- _A, b <- _A]

hasIdentity :: Eq a => [a] -> a -> (a -> a -> a) -> Bool
hasIdentity _A e op
  | not $ elem e _A = False                               -- e <- A
  | otherwise = all (\a -> op e a == a && op a e == a) _A -- e * a = a * e = a, a <- A

isInvertible :: Eq a => [a] -> a -> (a -> a) -> (a -> a -> a) -> Bool
isInvertible _A e inv op
 | not $ all (\(_, i) -> elem i _A) pairs = False                -- -a <- A, a <- A
 | otherwise = all (\(a, i) -> op a i == e && op i a == e) pairs -- a * i = i * a = e, a <- A
  where pairs = [(a, inv a) | a <- _A]