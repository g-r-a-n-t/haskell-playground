module Gec.Order where

import Gec.ModularArithmetic
import Gec.Types
import Test.HUnit

naive :: Curve -> Integer
naive (Curve a b p) =
  let ys = foldl (\acc x -> acc ++ (quadraticResidues (x^3 + a*x + b) p)) [] [0..p-1]
  in fromIntegral (length ys) + 1

runTests = runTestTT tests
  where tests = TestList[
                  TestCase (assertEqual "naive" 196 (naive (Curve 73 43 197))),
                  TestCase (assertEqual "naive" 100 (naive (Curve 2 3 97)))
                  ]
