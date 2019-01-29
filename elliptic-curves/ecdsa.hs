module Gec.ECDSA where

import Test.HUnit
import Gec.ModularArithmetic
import Gec.Types

-- ECDSA --

-- sign(domain, k, da, z) -> (r, s)
sign :: Domain -> Integer -> Integer -> Integer -> (Integer, Integer)
sign d k da z =
  let (Domain c _G n h) = d
      (x, _) = pointScale c k _G
      r = x `mod` n
      s = (mInverse k n) * (z + r*da) `mod` n
  in (r, s)

-- verify(domain H z (r, s)) -> isValid
verify :: Domain -> (Integer, Integer) -> Integer -> (Integer, Integer) -> Bool
verify d _Ha z (r,s) =
  let (Domain c g n _) = d
      (Curve _ _ p) = c
      u1 = ((mInverse s n) * z) `mod` n
      u2 = ((mInverse s n) * r) `mod` n
      (xp, _) = pointAdd c (pointScale c u1 g) (pointScale c u2 _Ha)
  in r == xp `mod` n

-- Tests --
runTests = runTestTT tests
  where curve = Curve 5 19 97
        _G = (18,11)
        domain = Domain curve _G 107 1
        k = 45
        da = 32
        z = 42
        _Ha = pointScale curve da _G
        tests = TestList [
          TestCase (assertEqual "signature creation" (39,100) (sign domain k da z)),
          TestCase (assertEqual "signature verification" True (verify domain _Ha z (39,100)))
          ]
