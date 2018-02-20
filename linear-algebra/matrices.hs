module Gla.Matrices where

import Gla.Vectors
import Math.Polynomial

-- conversions
numMatrixToPolyMatrix :: [[Float]] -> [[Poly Float]]
numMatrixToPolyMatrix m = map (\v -> numsToPolys v) m

polyMatrixToNumMatrix :: [[Poly Float]] -> [[Float]]
polyMatrixToNumMatrix m = map (\v -> polysToNums v) m

-- misc
removeN n xs = let (ys,zs) = splitAt n xs in ys ++ (tail zs) 
emptyMatrix n = take n $ repeat []
getRow m i = m!!i
getCol m j = map (\v -> v!!j) m

-- scale
scale :: Poly Float -> [[Poly Float]] -> [[Poly Float]]
scale s m = map (\v -> Gla.Vectors.scale s v) m

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

-- multiply
multiply :: [[Poly Float]] -> [[Poly Float]] -> [[Poly Float]]
multiply a b =  

---- transform
--transform :: Num a => [[a]] -> [a] -> [a]
--transform m v1 = map (\v2 -> Gla.Vectors.dot v1 v2) m
--
---- identity
--zeroVector n = take n $ repeat 0
--replaceN n x l = take n l ++ [x] ++ drop (n + 1) l
--
--identity :: Int -> [[Int]]
--identity n = map (\i -> replaceN i 1 (zeroVector n)) [0..(n-1)]
--
---- cofactor
--cofactorValue (l,t) m = m!!t!!l * (-1)^(l+t)
--cofactor :: Num a => [[a]] -> [[a]]
--cofactor m = map (\t -> map (\l -> cofactorValue (l,t) m) [0..(length m) - 1]) [0..(length m) - 1]
--
---- minors
--minor :: Num a => (Int, Int) -> [[a]] -> a
--minor (l,t) m = determinant (map (\v -> removeN l v) (removeN t m))
--
--minors :: Num a => [[a]] -> [[a]]
--minors [[a,b],[c,d]] = [[d,c],[b,a]]
--minors m = map (\t -> map (\l -> minor (l,t) m) [0..(length m) - 1]) [0..(length m) - 1]
--
---- inverse
--inverse :: Fractional a => [[a]] -> [[a]]
--inverse m = Gla.Matrices.scale (1 / (determinant m)) (mirror  (cofactor (minors m)))
--
---- eigenvalues
---- polyMatrix :: [[Integer]] -> [[Math.Polynomial.Poly Integer]]
--pmatrix m d = map (\v -> map (\x -> poly BE ([x] ++ (zeroVector d))) v) m
--padd m1 m2 = map (\(a,b) -> Gla.Vectors.padd a b) (zip m1 m2)
--identityDiff m = Gla.Matrices.padd (pmatrix m 0) (pmatrix (Gla.Matrices.scale (-1) (identity (length m))) 1) 
--pcofactorValue (l,z) m = scalePoly (-1)^(l+z) m!!z!!l
--pcofactor m = map (\z -> map (\l -> pcofactorValue (l,z) m) [0..((length m) - 1)]) [0..((length m) - 1)]
--pdeterminant [[a,b],[c,d]] = addPoly (multPoly a d)  (scalePoly (-1) (multPoly b c))
--pdeterminant m = sumPolys (map (\(s, m) -> scalePoly s (pdeterminant m)) (zip ((pcofactor m)!!0) (subMatrices m)))
--







