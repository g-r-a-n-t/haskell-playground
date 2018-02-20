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

-- dot product
dot :: [Poly Float] -> [Poly Float] -> Poly Float
dot v1 v2 = sumPolys (map (\(a,b) -> multPoly a b) (zip v1 v2))

-- addition
add :: [Poly Float] -> [Poly Float] -> [Poly Float]
add v1 v2 = map (\(a,b) -> addPoly a b) (zip v1 v2)

-- subtraction
subtract :: [Poly Float] -> [Poly Float] -> [Poly Float]
subtract a b = map (\(x,y) -> subPoly x y) (zip a b)

-- scale
scale :: Poly Float -> [Poly Float] -> [Poly Float]
scale s v = map (\a -> multPoly a s) v

