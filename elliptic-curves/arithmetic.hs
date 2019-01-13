module Gec.Arithmetic where

import Test.HUnit

-- helpers

-- Extended Euclidean Algorithm
-- taken from https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm#Extended_2
eGCD :: Integer -> Integer -> (Integer,Integer,Integer)
eGCD 0 b = (b, 0, 1)
eGCD a b = let (g, s, t) = eGCD (b `mod` a) a
           in (g, t - (b `div` a) * s, s)

-- Modular inversemod
-- ax = 1 (mod m)
mInverse :: Integer -> Integer -> Integer
mInverse n p
  | x*n + y*p == 1 && gcd == 1 = x `mod` p
  | otherwise = 0 -- TODO: throw an error here
  where (gcd, x, y) = eGCD n p


-- arithmetic

-- modular slope at single point
mTangent :: (Integer, Integer, Integer) -> (Integer, Integer) -> Integer
mTangent (a, b, p) (x, y) = (3 * x^2 + a) * (mInverse (2 * y) p) `mod` p

mSlope :: (Integer, Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Integer
mSlope (a, b, p) (xp, yp) (xq, yq) = (yp - yq) * (mInverse ((xp - xq) `mod` p) p) `mod` p

-- modular point addition
mPointAdd :: (Integer, Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mPointAdd (a, b, p) (xp, yp) (xq, yq)
  | (xp, yp) == (-xq, -yq) = (0, 0) -- TODO: find a better way to represent the point at infinity
  | otherwise              =
    let m  = if (xp, yp) == (xq, yq) then mTangent (a, b, p) (xp, yp) else mSlope (a, b, p) (xp, yp) (xq, yq)
        xr = (m^2 - xp - xq) `mod` p
        yr = (yp + m * (xr - xp)) `mod` p
    in (xr, yr)


domain = (2, 3, 97)

tests = TestList [
  TestCase (assertEqual "modular inverse of a = 9 and p = 23" 18 (mInverse 9 23)),
  TestCase (assertEqual "modular inverse of a = 3 and p = 26" 9  (mInverse 3 26)),
  TestCase (assertEqual "modular point addition" (11, 80) (mPointAdd domain (3, 6) (3, 6)))
  ]

runTests = runTestTT (tests)
