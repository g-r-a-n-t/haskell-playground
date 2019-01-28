module Gec.ModularArithmetic where

import Test.HUnit
import Gec.Types

-- helpers --

-- Extended Euclidean Algorithm
-- taken from https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm#Extended_2
eGCD :: Integer -> Integer -> (Integer,Integer,Integer)
eGCD 0 b = (b, 0, 1)
eGCD a b = let (g, s, t) = eGCD (b `mod` a) a
           in (g, t - (b `div` a) * s, s)

toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin n = let (q,r) = n `divMod` 2 in r : toBin q

-- Modular inverse
-- ax = 1 (mod n)
mInverse :: Integer -> Integer -> Integer
mInverse a n
  | x*a' + y*n == 1 && gcd == 1 = x `mod` n
  | otherwise = error "unable to compute inverse"
  where a' = a `mod` n
        (gcd, x, y) = eGCD a' n

-- modular slope at single point
mTangent :: Curve -> (Integer, Integer) -> Integer
mTangent (Curve a b n) (x, y) = (3 * x^2 + a) * (mInverse (2 * y) n) `mod` n

-- modular slope between two points
mSlope :: Curve -> (Integer, Integer) -> (Integer, Integer) -> Integer
mSlope (Curve a b n) (xp, yp) (xq, yq) = ((yp - yq) * (mInverse (xp - xq) n)) `mod` n


pointDoubledNTimes :: Curve -> Integer -> (Integer, Integer) -> (Integer, Integer)
pointDoubledNTimes c n p -- TODO: Memoize this
  | n == 0 = p
  | otherwise =
    let previousPoint = pointDoubledNTimes c (n - 1) p
    in pointAdd c previousPoint previousPoint

-- Arithmetic --

inf = -1 -- TODO: find a better way to handle infinity

-- modular point addition
pointAdd :: Curve -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
pointAdd c p q
  | (xp, yp) == (inf, inf) = (xq, yq) -- P = 0
  | (xq, yq) == (inf, inf) = (xp, yp) -- Q = 0
  | (xp, yp) == (xq, (-yq) `mod` n) = (inf, inf)
  | otherwise =
    let m  = if p == q then mTangent c p else mSlope c p q
        xr = (m^2 - xp - xq) `mod` n
        yr = (-(yp + m * (xr - xp))) `mod` n
    in (xr, yr)
  where (Curve a b n) = c
        (xp, yp)      = p
        (xq, yq)      = q

pointScale :: Curve -> Integer -> (Integer, Integer) -> (Integer, Integer)
pointScale c s p
  | s == 0 = (inf, inf)
  | otherwise =
    let indices = reverse (foldl (\acc (b, i) -> if b == 1 then acc ++ [i] else acc) [] (zip (toBin s) [0..]))
        doubles = map (\i -> pointDoubledNTimes c i p) indices
    in foldl (\acc double -> pointAdd c acc double) (head doubles) (tail doubles)

-- Tests --

testCurve = Curve 2 3 97

moudlarArithmeticTests = TestList [
  TestCase (assertEqual "modular inverse" 18 (mInverse 9 23)),
  TestCase (assertEqual "modular inverse" 9  (mInverse 3 26)),
  TestCase (assertEqual "modular slope" 32  (mSlope testCurve (3, 6) (12, 3))),
  TestCase (assertEqual "modular point addition" (39, 6) (pointAdd testCurve (3, 6) (12, 3))),
  TestCase (assertEqual "modular point addition" (24, 2) (pointAdd testCurve (12, 3) (12, 3))),
  TestCase (assertEqual "modular point addition" (54, 12) (pointAdd testCurve (3, 6) (22, 5))),
  TestCase (assertEqual "modular point addition" (12, 94) (pointAdd testCurve (22, 5) (32, 7))),
  TestCase (assertEqual "modular point addition" (12, 94) (pointAdd testCurve (32, 7) (22, 5))),
  TestCase (assertEqual "modular point addition" (21, 24) (pointAdd testCurve (22, 5) (22, 5))),
  TestCase (assertEqual "modular point addition" (80, 10) (pointAdd testCurve (3, 6) (3, 6))),
  TestCase (assertEqual "modular point addition" (3, 6) (pointAdd testCurve (3, 6) (inf, inf))),
  TestCase (assertEqual "modular point scaling" (80, 10) (pointScale testCurve 347 (3, 6))),
  TestCase (assertEqual "modular point scaling" (3, 6) (pointScale testCurve 21 (3, 6))),
  TestCase (assertEqual "modular point scaling" (inf, inf) (pointScale testCurve 20 (3, 6)))
  ]

runModularArithmeticTests = runTestTT moudlarArithmeticTests
