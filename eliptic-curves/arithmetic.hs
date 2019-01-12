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
-- ax = 1 (mod m)
mInverse :: Integer -> Integer -> Integer
mInverse a m
  | x*a + y*m == 1 && gcd == 1 = x `mod` m
  | otherwise = 0 -- TODO: throw an error here
  where (gcd, x, y) = eGCD a m


-- arithmetic

-- modular slope at single point
mSlopeAtPoint :: (Integer, Integer, Integer) -> (Integer, Integer) -> Integer
mSlopeAtPoint (a, b, p) (x, y)


-- modular point addition
mPointAdd :: (Integer, Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mPointAdd (a, b, p) (xp, yp) (xq, yq)
  | (xp, yp) == (-xq, -yq) = 0
  | (xp, yp) != (xq, yq)   =
  where

domain = (2, 3, 97)

tests = TestList [
  TestCase (assertEqual "modular inverse of a = 9 and p = 23" 18 (mInverse 9 23)),
  TestCase (assertEqual "modular inverse of a = 9 and p = 23" 9  (mInverse 3 26)),
  TestCase (assertEqual "modular point addition" (25, 62) (mPointAdd domain (3, 6) (4, 47)))
  ]

runTests = runTestTT (tests)
