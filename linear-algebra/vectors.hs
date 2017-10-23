module Gla.Vectors where

-- dot product
dot :: [Int] -> [Int] -> Int
dot v1 v2 = sum (map (\(a,b) -> a * b) (zip v1 v2))

-- addition
add :: [Int] -> [Int] -> [Int]
add v1 v2 = map (\(a,b) -> a + b) (zip v1 v2)

-- subtraction
subtract :: [Int] -> [Int] -> [Int]
subtract v1 v2 = map (\(a,b) -> a - b) (zip v1 v2)

-- scale
scale :: Int -> [Int] -> [Int]
scale s v = map (\a -> a * s) v
