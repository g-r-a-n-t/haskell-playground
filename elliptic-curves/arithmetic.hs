module Gec.Arithmetic where

import Test.HUnit

-------------
-- helpers --
-------------

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
mTangent :: (Integer, Integer, Integer) -> (Integer, Integer) -> Integer
mTangent (a, b, n) (x, y) = (3 * x^2 + a) * (mInverse (2 * y) n) `mod` n

-- modular slope between two points
mSlope :: (Integer, Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Integer
mSlope (a, b, n) (xp, yp) (xq, yq) = ((yp - yq) * (mInverse (xp - xq) n)) `mod` n

----------------
-- arithmetic --
----------------

inf = -1 -- TODO: find a better way to handle infinity

-- modular point addition
mPointAdd :: (Integer, Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mPointAdd d p q
  | (xp, yp) == (inf, inf) = (xq, (-yq) `mod` n) -- P = 0
  | (xq, yq) == (inf, inf) = (xp, (-yp) `mod` n) -- Q = 0
  | (xp, yp) == (xq, (-yq) `mod` n) = (inf, inf)
  | otherwise =
    let m  = if p == q then mTangent d p else mSlope d p q
        xr = (m^2 - xp - xq) `mod` n
        yr = (-(yp + m * (xr - xp))) `mod` n
    in (xr, yr)
  where (a, b, n) = d
        (xp, yp)  = p
        (xq, yq)  = q

mPointScale :: (Integer, Integer, Integer) -> Integer -> (Integer, Integer) -> (Integer, Integer)
mPointScale d s p
  | s == 0 = (inf, inf)
  | otherwise =
    let bits    = toBin s
        nBits   = length bits
        doubles = map (\n -> pointDoubledNTimes d n p) [0..nBits]
    in foldl (\acc (bit, double) -> if bit == 1 then mPointAdd d acc double else acc) p (zip binary doubles)


pointDoubledNTimes :: (Integer, Integer, Integer) -> Integer -> (Integer, Integer) -> (Integer, Integer)
pointDoubledNTimes d n p -- TODO: Memoize this
  | n == 0 = p
  | otherwise =
    let previousPoint = pointDoubledNTimes d (n - 1) p
    in mPointAdd d previousPoint previousPoint

-----------
-- tests --
-----------

domain = (2, 3, 97)

tests = TestList [
  TestCase (assertEqual "modular inverse" 18 (mInverse 9 23)),
  TestCase (assertEqual "modular inverse" 9  (mInverse 3 26)),
  TestCase (assertEqual "modular slope" 32  (mSlope domain (3, 6) (12, 3))),
  TestCase (assertEqual "modular point addition" (39, 6) (mPointAdd domain (3, 6) (12, 3))),
  TestCase (assertEqual "modular point addition" (24, 2) (mPointAdd domain (12, 3) (12, 3))),
  TestCase (assertEqual "modular point addition" (54, 12) (mPointAdd domain (3, 6) (22, 5))),
  TestCase (assertEqual "modular point addition" (12, 94) (mPointAdd domain (22, 5) (32, 7))),
  TestCase (assertEqual "modular point addition" (12, 94) (mPointAdd domain (32, 7) (22, 5))),
  TestCase (assertEqual "modular point addition" (21, 24) (mPointAdd domain (22, 5) (22, 5))),
  TestCase (assertEqual "modular point addition" (80, 10) (mPointAdd domain (3, 6) (3, 6)))
  ]

runTests = runTestTT (tests)
