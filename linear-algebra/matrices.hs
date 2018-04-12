module Gla.Matrices where

import Gla.Vectors
import Math.Polynomial
import Polynomial.Roots
import Data.Complex

-- conversions
numMatrixToPolyMatrix :: [[Float]] -> [[Poly Float]]
numMatrixToPolyMatrix m = map (\v -> numsToPolys v) m

polyMatrixToNumMatrix :: [[Poly Float]] -> [[Float]]
polyMatrixToNumMatrix m = map (\v -> polysToNums v) m

numToComplex x = x :+ 0
numsToComplexes xs = map (\x -> numToComplex x) xs

-- misc
removeN n xs = let (ys,zs) = splitAt n xs in ys ++ (tail zs) 
emptyMatrix n = take n $ repeat []
getRow m i = m!!i
getCol m j = map (\v -> v!!j) m
zeroVector n = take n $ repeat (numToPoly 0)
replaceN n x l = take n l ++ [x] ++ drop (n + 1) l
removeZeros xs = filter (/=0) xs

-- roots
findRoots :: Poly Float -> [Float]
findRoots p = removeZeros (map (\n -> realPart n) (roots 1e-16 1000 (numsToComplexes (polyCoeffs LE p))))

-- scale
scaleMatrix :: Poly Float -> [[Poly Float]] -> [[Poly Float]]
scaleMatrix s m = map (\v -> Gla.Vectors.scale s v) m

-- minor
minor :: [[Poly Float]] -> Int -> Int -> [[Poly Float]]
minor m i j = removeN i (map (\v -> removeN j v) m)

-- cofactor
cofactor :: [[Poly Float]] -> Int -> Int -> Poly Float
cofactor m i j = scalePoly ((-1)^(i+j)) (determinant (minor m i j))

-- determinant
determinant :: [[Poly Float]] -> Poly Float
determinant [[a,b],[c,d]] = subPoly (multPoly a d) (multPoly b c)
determinant m = sumPolys (map (\j -> multPoly (m!!0!!j) (cofactor m 0 j)) [0..(length m) - 1])

-- mirror
mirror :: [[Poly Float]] -> [[Poly Float]]
mirror m = map (\j -> getCol m j) [0..(length (m!!0))-1]

-- transform
transform :: [Poly Float] -> [[Poly Float]] -> [Poly Float]
transform v1 m = map (\v2 -> dot v1 v2) m

-- multiply
multiplyMatrices :: [[Poly Float]] -> [[Poly Float]] -> [[Poly Float]]
multiplyMatrices m1 m2 = map (\v -> transform v (mirror m2)) m1

-- identity
identity :: Int -> [[Poly Float]]
identity n = map (\i -> replaceN i (numToPoly 1) (zeroVector n)) [0..(n-1)]

-- comatrix
--comatrix :: [[Poly Float]] -> [[Poly Float]]
--comatrix m = map (\(v,i) -> map (\(_,j) -> cofactor m i j) (zip v [0..])) (zip m [0..])

-- addition
addMatrix :: [[Poly Float]] -> [[Poly Float]] -> [[Poly Float]]
addMatrix m1 m2 = map (\(v1,v2) -> Gla.Vectors.add v1 v2) (zip m1 m2)

-- subtraction
subtractMatrix :: [[Poly Float]] -> [[Poly Float]] -> [[Poly Float]]
subtractMatrix m1 m2 = map (\(v1,v2) -> Gla.Vectors.subtract v1 v2) (zip m1 m2)

-- inverse
--inverse :: [[Poly Float]] -> [[Poly Float]]
--inverse m = scaleMatrix (powPoly (determinant m) (-1)) (mirror (comatrix m))

-- eigenvalues
eigenvalues :: [[Poly Float]] -> [Float]
eigenvalues m = findRoots (determinant (subtractMatrix m (scaleMatrix x (identity (length m)))))






