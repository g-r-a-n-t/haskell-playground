# Grant's Linear Algebra Library

## Supported operations
### Gla.Vectors
- `addV [1,2] [3,4] --> [3,6]`
- `subV [1,2] [3,4] --> [-2,-2]`
- `dot [1,2] [3,4] --> 11`
- `scaleV 2 [3,4] --> [6,8]`

### Gla.Matrices
- `scaleM 2 [[1,2],[3,4]] --> [[2,4],[6,8]]`
- `minor [[1,2,2],[3,4,4],[5,6,7]] 1 1 --> 3`
- `cofactor [[1,2,2],[3,4,4],[5,6,7]] 1 1 --> -3`
-  cofactorMatrix [[1,2,2],[3,4,4],[5,6,7]] -> [[4,-2,0],[-1,-3,2],[-2,4,-2]]
- `determinant [[1,2,3],[4,5,6],[7,8,9]] --> 0`
- `mirror [[1,2],[3,4]] --> [[1,3],[2,4]]`
- `transform [1,2] [[1,2],[3,4]] --> [5,11]`
- `mult [[1,2],[3,4]] [[5,6],[7,8]] --> [[19,22],[43,50]]`
- `identity 3 --> [[1,0,0],[0,1,0],[0,0,1]]`
- `addM [[1,2],[3,4]] [[1,1],[1,1]] --> [[2,3],[4,5]]`
- `subM [[1,2],[3,4]] [[1,1],[1,1]] --> [[0,1],[2,3]]`
- `inverse [[1,2,3],[5,5,6],[8,8,9]] --> [[-1,2,-1],[1,-5,3],[0,2.7,-1.7]]`
- `eigenvalues [[2,0,0],[1,2,1],[-1,0,1]] --> [2,1]`

## Required Libraries
- Math.Polynomial
- Polynomial.Roots

## Running Tests
```
$ ghci vector_tests vectors
> runTests

$ ghci matrix_tests matrices vectors
> runTests
```
