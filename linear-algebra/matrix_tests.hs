module Gla.MatrixTests where

import Gla.Matrices
import Gla.Vectors
import Test.HUnit
import Math.Polynomial

vector1_2_3 = numsToPolys [1,2,3]
vector11_23_38 = numsToPolys [11,23,38]
matrix2x2_1 = numMatrixToPolyMatrix [[1,2],[3,4]]
matrix2x2_2 = numMatrixToPolyMatrix [[1,1],[1,1]]
matrix2x2_3 = numMatrixToPolyMatrix [[0,1],[2,3]]
matrix2x2_4 = numMatrixToPolyMatrix [[2,4],[6,8]]
matrix2x2_5 = numMatrixToPolyMatrix [[1,2],[5,7]]
matrix2x2_6 = numMatrixToPolyMatrix [[1,5],[2,7]]
matrix2x2_7 = numMatrixToPolyMatrix [[4,7],[8,15]]
matrix3x2_1 = numMatrixToPolyMatrix [[1,4],[2,3],[1,3]]
matrix3x2_2 = numMatrixToPolyMatrix [[7,16],[15,36],[24,59]]
matrix3x3_1 = numMatrixToPolyMatrix [[1,2,2],[3,4,4],[5,6,7]]
matrix4x4_1 = numMatrixToPolyMatrix [[1,2,3,4],[2,2,2,2],[3,4,1,1],[5,3,3,2]]
identity2 = numMatrixToPolyMatrix [[1,0],[0,1]]
identity3 = numMatrixToPolyMatrix [[1,0,0],[0,1,0],[0,0,1]]
identity4 = numMatrixToPolyMatrix [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]

tests = TestList [
  TestCase (assertEqual "nums to polys test 1" [[numToPoly 1,numToPoly 2],[numToPoly 3, numToPoly 4]] matrix2x2_1),
  TestCase (assertEqual "polys to nums test 2" [[1,2],[3,4]] (polyMatrixToNumMatrix matrix2x2_1)),
  TestCase (assertEqual "scale test 1" matrix2x2_4 (scaleMatrix (numToPoly 2) matrix2x2_1)),
  TestCase (assertEqual "minor test 1" matrix2x2_1 (minor matrix3x3_1 2 2)),
  TestCase (assertEqual "minor test 2" matrix2x2_5 (minor matrix3x3_1 1 1)),
  TestCase (assertEqual "cofactor test 1" (numToPoly (-3)) (cofactor matrix3x3_1 1 1)),
  TestCase (assertEqual "determinant test 1" (numToPoly (-8)) (determinant matrix2x2_4)),
  TestCase (assertEqual "determinant test 2" (numToPoly (-2)) (determinant matrix3x3_1)),
  TestCase (assertEqual "determinant test 3" (numToPoly 4) (determinant matrix4x4_1)),
  TestCase (assertEqual "mirror test 1" matrix2x2_6 (mirror matrix2x2_5)),
  TestCase (assertEqual "transform test 1" vector11_23_38 (transform vector1_2_3 matrix3x3_1)),
  TestCase (assertEqual "multiply test 1" matrix2x2_7 (multiplyMatrices matrix2x2_1 matrix2x2_3)),
  TestCase (assertEqual "multiply test 2" matrix3x2_2 (multiplyMatrices matrix3x3_1 matrix3x2_1)),
  TestCase (assertEqual "identity test 1"  identity2 (identity 2)),
  TestCase (assertEqual "identity test 2"  identity3 (identity 3)),
  TestCase (assertEqual "identity test 3"  identity4 (identity 4)),
  TestCase (assertEqual "add test 1"  matrix2x2_1 (addMatrix matrix2x2_2 matrix2x2_3)),
  TestCase (assertEqual "subtract test 1"  matrix2x2_3 (subtractMatrix matrix2x2_1 matrix2x2_2)),
  TestCase (assertEqual "for commas" True True)
  ]

runTests = runTestTT (tests)
