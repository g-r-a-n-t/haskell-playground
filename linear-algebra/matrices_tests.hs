module Gla.Tests where

import Gla.Matrices
import Test.HUnit

matrix2x2_1 = [[1,2],[3,4]]
matrix2x2_2 = [[5,6],[7,8]]
matrix3x3_1 = [[1,2,3],[4,5,6],[7,7,9]]
matrix3x3_2 = [[1,2,3],[4,5,6],[7,8,9]]
matrix3x3_3 = [[2,1,2],[2,1,4],[4,4,4]]
identity2 = [[1,0],[0,1]]
identity3 = [[1,0,0],[0,1,0],[0,0,1]]

tests = TestList [
  TestCase (assertEqual "determinant test 1" (-2) (determinant matrix2x2_1)),
  TestCase (assertEqual "determinant test 2" (-6) (determinant matrix3x3_1)),
  TestCase (assertEqual "mirror test 1" [[1,4,7],[2,5,8],[3,6,9]] (mirror matrix3x3_2)),
  TestCase (assertEqual "scale test 1" [[2,4,6],[8,10,12],[14,16,18]] (scale 2 matrix3x3_2)),
  TestCase (assertEqual "multiply test 1" [[19,22],[43,50]] (multiply matrix2x2_1 matrix2x2_2)),
  TestCase (assertEqual "multiply test 2" [[30,36,42],[66,81,96],[98,121,144]] (multiply matrix3x3_1 matrix3x3_2)),
  TestCase (assertEqual "identity test 1" identity2 (identity 2)),
  TestCase (assertEqual "identity test 2" identity3 (identity 3)),
  TestCase (assertEqual "checker test 1" [[1,-2,3],[-4,5,-6],[7,-8,9]] (cofactor matrix3x3_2)),
  TestCase (assertEqual "inverse test 1" [[1.5,-0.5,-0.25],[-1,0,0.5],[-0.5,0.5,0]] (inverse matrix3x3_3))
  ]

runTests = runTestTT (tests)
