module Gla.Vectors where

import Math.Polynomial

-- conversions
numToPoly :: Float -> Poly Float
numToPoly x = poly BE [x]

polyToNum :: Poly Float -> Float
polyToNum p = evalPoly p 0

numsToPolys :: [Float] -> [Poly Float]
numsToPolys v = map (\x -> numToPoly x) v

polysToNums :: [Poly Float] -> [Float]
polysToNums v = map (\p -> polyToNum p) v

--misc
subPoly :: Poly Float -> Poly Float -> Poly Float
subPoly a b = addPoly a (scalePoly (-1) b)

-- invert polynomial (only works with constants: 4 -> 1/4, 2x + 4 -> 1/4)
invertPoly :: Poly Float -> Poly Float
invertPoly p = numToPoly (1 / polyToNum(p))

-- dot product
dot :: [Poly Float] -> [Poly Float] -> Poly Float
dot v1 v2 = sumPolys (map (\(a,b) -> multPoly a b) (zip v1 v2))

-- addition
addV :: [Poly Float] -> [Poly Float] -> [Poly Float]
addV v1 v2 = map (\(a,b) -> addPoly a b) (zip v1 v2)

-- subtraction
subV :: [Poly Float] -> [Poly Float] -> [Poly Float]
subV a b = map (\(x,y) -> subPoly x y) (zip a b)

-- scale
scaleV :: Poly Float -> [Poly Float] -> [Poly Float]
scaleV s v = map (\a -> multPoly a s) v
