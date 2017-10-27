module Gla.Vectors where

-- dot product
dot :: Num a => [a] -> [a] -> a
dot v1 v2 = sum (map (\(a,b) -> a * b) (zip v1 v2))

-- addition
add :: Num a => [a] -> [a] -> [a]
add v1 v2 = map (\(a,b) -> a + b) (zip v1 v2)

-- subtraction
subtract :: Num a => [a] -> [a] -> [a]
subtract v1 v2 = map (\(a,b) -> a - b) (zip v1 v2)

-- scale
scale :: Num a => a -> [a] -> [a]
scale s v = map (\a -> a * s) v
