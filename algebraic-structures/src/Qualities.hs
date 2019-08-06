module Qualities (
  hasClosure,
  isAssociative,
  isCommunicative
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

