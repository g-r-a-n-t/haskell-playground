module Gla.Vectors where

import Math.Polynomial

-- conversions
numToPoly :: (Num a, Eq a) => a -> Poly a
numToPoly x = poly BE [x]

polyToNum :: (Num a, Eq a) => Poly a -> a
polyToNum p = evalPoly p 0

numsToPolys :: (Num a, Eq a) => [a] -> [Poly a]
numsToPolys v = map (\x -> numToPoly x) v

polysToNums :: (Num a, Eq a) => [Poly a] -> [a]
polysToNums v = map (\p -> polyToNum p) v

-- dot product
dot :: (Num a, Eq a) => [Poly a] -> [Poly a] -> Poly a
dot v1 v2 = sumPolys (map (\(a,b) -> multPoly a b) (zip v1 v2))

-- addition
add :: (Num a, Eq a) => [Poly a] -> [Poly a] -> [Poly a]
add v1 v2 = map (\(a,b) -> addPoly a b) (zip v1 v2)

-- subtraction
subtract :: (Num a, Eq a) => [Poly a] -> [Poly a] -> [Poly a]
subtract v1 v2 = map (\(a,b) -> addPoly a (scalePoly (-1) b)) (zip v1 v2)

-- scale
scale :: (Num a, Eq a) => Poly a -> [Poly a] -> [Poly a]
scale s v = map (\a -> multPoly a s) v

