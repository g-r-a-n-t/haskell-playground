module Gla.VectorTests where

import Gla.Vectors
import Test.HUnit

tests = TestList [
  TestCase (assertEqual "dot test 1" 11 (dot [1,2] [3,4])),
  TestCase (assertEqual "dot test 2" 32 (dot [1,2,3] [4,5,6])),
  TestCase (assertEqual "addition test 1" [4,6] (add [1,2] [3,4])),
  TestCase (assertEqual "subtraction test 1" [-2,-2] (Gla.Vectors.subtract [1,2] [3,4])),
  TestCase (assertEqual "scale test 1" [6,8] (scale 2 [3,4]))
  ]

runTests = runTestTT (tests)
