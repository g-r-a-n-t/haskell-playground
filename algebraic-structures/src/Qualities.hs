module Qualities (
  hasClosure,
  isAssociative,
  isCommunicative,
  hasIdentity,
  isInvertible
) where

hasClosure :: Eq a => [a] -> (a -> a -> a) -> Bool
hasClosure _S op = and [elem (op a b) _S | (a, b) <- pairs]
                   where pairs = [(a, b) | a <- _S, b <- _S]

isAssociative :: Eq a => [a] -> (a -> a -> a) -> Bool
isAssociative _S op = and [op (op a b) c == op a (op b c) | (a, b, c) <- triplets]
                      where triplets = [(a, b, c) | a <- _S, b <- _S, c <- _S]

isCommunicative :: Eq a => [a] -> (a -> a -> a) -> Bool
isCommunicative _S op = and [op a b == op b a | (a, b) <- pairs]
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