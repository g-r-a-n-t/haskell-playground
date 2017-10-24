module Gla.Matrices where

import Gla.Vectors

-- determinant
negateOdds :: [Int] -> [Int]
negateOdds list = map (\(x,y) -> x * y) (zip (map (\x -> (-1)^x) [0..(length list) - 1]) list)

subMatrices :: [[Int]] -> [[[Int]]]
subMatrices matrix = foldr (\n accum -> subMatrix n matrix : accum) [] [0..(length matrix) - 1]

subMatrix :: Int -> [[Int]] -> [[Int]]
subMatrix n (first:rest) = map (\xs -> removeN n xs) rest

removeN n xs = let (ys,zs) = splitAt n xs   in   ys ++ (tail zs) 

determinant :: [[Int]] -> Int
determinant [[a,b],[c,d]] = a * d - b * c
determinant matrix = sum (map (\(scalar, matrix) -> scalar * (determinant matrix)) (zip (negateOdds (matrix!!0)) (subMatrices matrix)))

-- mirror
spread :: [[Int]] -> [Int] -> [[Int]]
spread l1 l2 = map (\(a,b) -> a : l1!!b) (zip l2 [0..])

emptyMatrix n = take n $ repeat []

mirror :: [[Int]] -> [[Int]]
mirror matrix = foldr (\v accum -> spread accum v) (emptyMatrix (length (matrix!!0))) matrix

-- multiply
dots :: [Int] -> [[Int]] -> [Int]
dots v1 m = map (\v2 -> Gla.Vectors.dot v1 v2) m 

multiply :: [[Int]] -> [[Int]] -> [[Int]]
multiply m1 m2 = map (\v1 -> dots v1 (mirror m2) ) m1

-- identity
zeroVector n = take n $ repeat 0
replaceN n x l = take n l ++ [x] ++ drop (n + 1) l

identity :: Int -> [[Int]]
identity n = map (\i -> replaceN i 1 (zeroVector n)) [0..(n-1)]

