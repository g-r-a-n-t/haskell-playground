module Gla.VectorTests where

import Gla.Vectors
import Test.HUnit

n11 = numToPoly 11
n32 = numToPoly 32
n2 = numToPoly 2

v1_2 = numsToPolys [1,2]
v3_4 = numsToPolys [3,4]
v4_6 = numsToPolys [4,6]
v6_8 = numsToPolys [6,8]
vn2_n2 = numsToPolys [-2,-2]
v1_2_3 = numsToPolys [1,2,3]
v4_5_6 = numsToPolys [4,5,6]


tests = TestList [
  TestCase (assertEqual "dot test 1" n11 (Gla.Vectors.dot v1_2 v3_4)),
  TestCase (assertEqual "dot test 2" n32 (dot v1_2_3 v4_5_6)),
  TestCase (assertEqual "addition test 1" v4_6 (add v1_2 v3_4)),
  TestCase (assertEqual "subtraction test 1" vn2_n2 (Gla.Vectors.subtract v1_2 v3_4)),
  TestCase (assertEqual "scale test 1" v6_8 (scale n2 v3_4))
  ]

runTests = runTestTT (tests)
