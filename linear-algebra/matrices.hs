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
roundF x = (fromInteger $ round $ x * (10^2)) / (10.0^^2)
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

-- roots
findRoots :: Poly Float -> [Float]
findRoots p = removeDuplicates (map (\n -> roundF (realPart n)) (roots 1e-16 1000 (numsToComplexes (polyCoeffs LE p))))

-- scale
scaleM :: Poly Float -> [[Poly Float]] -> [[Poly Float]]
scaleM s m = map (\v -> scaleV s v) m

-- minor
minor :: [[Poly Float]] -> Int -> Int -> Poly Float
minor m i j = determinant (removeN i (map (\v -> removeN j v) m))

-- cofactor
cofactor :: [[Poly Float]] -> Int -> Int -> Poly Float
cofactor m i j = scalePoly ((-1)^(i+j)) (minor m i j)

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
multM :: [[Poly Float]] -> [[Poly Float]] -> [[Poly Float]]
multM m1 m2 = map (\v -> transform v (mirror m2)) m1

-- identity
identity :: Int -> [[Poly Float]]
identity n = map (\i -> replaceN i (numToPoly 1) (zeroVector n)) [0..(n-1)]

-- cofactor or adjunct matrix
cofactorMatrix :: [[Poly Float]] -> [[Poly Float]]
cofactorMatrix m = map (\(v,i) -> map (\(_,j) -> cofactor m j i) (zip v [0..])) (zip m [0..])

-- addition
addM :: [[Poly Float]] -> [[Poly Float]] -> [[Poly Float]]
addM m1 m2 = map (\(v1,v2) -> addV v1 v2) (zip m1 m2)

-- subtraction
subM :: [[Poly Float]] -> [[Poly Float]] -> [[Poly Float]]
subM m1 m2 = map (\(v1,v2) -> subV v1 v2) (zip m1 m2)

-- inverse (note: any coefficients are lost here)
inverse :: [[Poly Float]] -> [[Poly Float]]
inverse m = scaleM (invertPoly (determinant m)) (cofactorMatrix m)

-- eigenvalues
eigenvalues :: [[Poly Float]] -> [Float]
eigenvalues m = findRoots (determinant (subM m (scaleM x (identity (length m)))))
