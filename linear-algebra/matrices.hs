module Gla.Matrices where

import Gla.Vectors

-- determinant
negateOdds :: Num a => [a] -> [a]
negateOdds list = map (\(x,y) -> x * y) (zip (map (\x -> (-1)^x) [0..(length list) - 1]) list)

subMatrices :: Num a => [[a]] -> [[[a]]]
subMatrices matrix = foldr (\n accum -> subMatrix n matrix : accum) [] [0..(length matrix) - 1]

subMatrix :: Num a => Int -> [[a]] -> [[a]]
subMatrix n (first:rest) = map (\xs -> removeN n xs) rest

removeN n xs = let (ys,zs) = splitAt n xs   in   ys ++ (tail zs) 

determinant :: Num a => [[a]] -> a
determinant [[a,b],[c,d]] = a * d - b * c
determinant matrix = sum (map (\(scalar, matrix) -> scalar * (determinant matrix)) (zip (negateOdds (matrix!!0)) (subMatrices matrix)))

-- mirror
spread l1 l2 = map (\(a,b) -> a : l1!!b) (zip l2 [0..])
emptyMatrix n = take n $ repeat []

mirror :: Num a => [[a]] -> [[a]]
mirror matrix = foldr (\v accum -> spread accum v) (emptyMatrix (length (matrix!!0))) matrix

-- scale
scale :: Num a => a -> [[a]] -> [[a]]
scale s m = map (\v -> Gla.Vectors.scale s v) m

-- multiply
dots :: Num a => [a] -> [[a]] -> [a]
dots v1 m = map (\v2 -> Gla.Vectors.dot v1 v2) m 

multiply :: Num a => [[a]] -> [[a]] -> [[a]]
multiply m1 m2 = map (\v1 -> dots v1 (mirror m2) ) m1

-- identity
zeroVector n = take n $ repeat 0
replaceN n x l = take n l ++ [x] ++ drop (n + 1) l

identity :: Int -> [[Int]]
identity n = map (\i -> replaceN i 1 (zeroVector n)) [0..(n-1)]

-- cofactor
cofactorValue (l,t) m = m!!t!!l * (-1)^(l+t)
cofactor :: Num a => [[a]] -> [[a]]
cofactor m = map (\t -> map (\l -> cofactorValue (l,t) m) [0..(length m) - 1]) [0..(length m) - 1]

-- minors
minor :: Num a => (Int, Int) -> [[a]] -> a
minor (l,t) m = determinant (map (\v -> removeN l v) (removeN t m))

minors :: Num a => [[a]] -> [[a]]
minors [[a,b],[c,d]] = [[d,c],[b,a]]
minors m = map (\t -> map (\l -> minor (l,t) m) [0..(length m) - 1]) [0..(length m) - 1]

-- inverse
inverse :: Fractional a => [[a]] -> [[a]]
inverse m = Gla.Matrices.scale (1 / (determinant m)) (mirror  (cofactor (minors m)))







