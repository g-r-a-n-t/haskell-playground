module Gla where

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

-- dot product
dot :: [Int] -> [Int] -> Int
dot v1 v2 = sum (map (\(a,b) -> a * b) (zip v1 v2))

-- vector addition
add :: [Int] -> [Int] -> [Int]
add v1 v2 = map (\(a,b) -> a + b) (zip v1 v2)

-- scale vector
scale :: Int -> [Int] -> [Int]
scale s v = map (\a -> a * s) v
