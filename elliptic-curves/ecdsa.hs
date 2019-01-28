module Gec.ECDSA where

import Test.HUnit
import Gec.ModularArithmetic
import Gec.Types

-- ECDSA --

-- sign(domain, k, da, z) -> (r, s)
sign :: Domain -> Integer -> Integer -> Integer -> (Integer, Integer)
sign d k da z =
  let (Domain c g n h) = d
      (x, _) = pointScale c k g
      r = x `mod` n
      s = (mInverse k n) * (z + r*da) `mod` n
  in (r, s)

-- verify(domain H z (r, s)) -> isValid
verify :: Domain -> (Integer, Integer) -> Integer -> (Integer, Integer) -> Bool
verify d h z (r,s) =
  let (Domain c g n _) = d
      (Curve _ _ p) = c
      u1 = ((mInverse s n) * z) `mod` n
      u2 = ((mInverse s n) * r) `mod` n
      (xp, _) = pointAdd c (pointScale c u1 g) (pointScale c u2 h)
  in r == xp `mod` n

-- Tests --
testDomain = Domain testCurve (3,6) 5 20

ecdsaTests = TestList [
  TestCase (assertEqual "signature creation" (28,16) (sign testDomain 3 2 2)),
  TestCase (assertEqual "signature verification" True (verify testDomain (28,63) 41 (28,16)))
  ]

runEcdsaTests = runTestTT ecdsaTests
