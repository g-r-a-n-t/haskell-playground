module Gec.Arithmetic where

import Test.HUnit

-- helpers

-- Extended Euclidean Algorithm
-- taken from https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm#Extended_2
eGCD :: Integer -> Integer -> (Integer,Integer,Integer)
eGCD 0 b = (b, 0, 1)
eGCD a b = let (g, s, t) = eGCD (b `mod` a) a
           in (g, t - (b `div` a) * s, s)

-- Modular inverse
-- ax = 1 (mod n)
mInverse :: Integer -> Integer -> Integer
mInverse a n
  | x*a' + y*n == 1 && gcd == 1 = x `mod` n
  | otherwise = 0 -- TODO: throw an error here
  where a'          = a `mod` n
        (gcd, x, y) = eGCD a' n


-- arithmetic

-- modular slope at single point
mTangent :: (Integer, Integer, Integer) -> (Integer, Integer) -> Integer
mTangent (a, b, p) (x, y) = (3 * x^2 + a) * (mInverse (2 * y) p) `mod` p

-- modular slope between two points
mSlope :: (Integer, Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Integer
mSlope (a, b, n) (xp, yp) (xq, yq) = ((yp - yq) * (mInverse (xp - xq) n)) `mod` n

-- modular point addition
mPointAdd :: (Integer, Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mPointAdd d p q
  | (xp, yp) == (xq, -yq) = (0, 0) -- TODO: find a better way to represent the point at infinity
  | otherwise              =
    let m  = if p == q then mTangent d p else mSlope d p q
        xr = (m^2 - xp - xq) `mod` n
        yr = (yp + m * (xr - xp)) `mod` n
    in (xr, (-yr) `mod` n)
  where (a, b, n) = d
        (xp, yp)  = p
        (xq, yq)  = q


domain = (2, 3, 97)

tests = TestList [
  TestCase (assertEqual "modular inverse" 18 (mInverse 9 23)),
  TestCase (assertEqual "modular inverse" 9  (mInverse 3 26)),
  TestCase (assertEqual "modular slope" 32  (mSlope domain (3, 6) (12, 3))),
  TestCase (assertEqual "modular point addition" (39, 6) (mPointAdd domain (3, 6) (12, 3))),
  TestCase (assertEqual "modular point addition" (24, 2) (mPointAdd domain (12, 3) (12, 3))),
  TestCase (assertEqual "modular point addition" (54, 12) (mPointAdd domain (3, 6) (22, 5))),
  TestCase (assertEqual "modular point addition" (12, 94) (mPointAdd domain (22, 5) (32, 7)))
  ]

runTests = runTestTT (tests)
