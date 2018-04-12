# Grant's Linear Algebra Library

Don't use this.

## Supported operations
### Gla.Vectors
- addition    `add [1,2] [3,4] --> [3,6]`
- subtraction `subtract [1,2] [3,4] --> [-2,-2]`
- dot product `dot [1,2] [3,4] --> 11`
- scaling     `scale 2 [3,4] --> [6,8]`

### Gla.Matrices
- scaling        `scale 2 [[1,2],[3,4]] --> [[2,4],[6,8]]`
- minor          `minor [[1,2,2],[3,4,4],[5,6,7]] 2 2 --> [[1,2],[3,4]]`
- cofactor       `cofactor [[1,2,2],[3,4,4],[5,6,7]] 1 1 --> -3`
- determinant    `determinant [[1,2,3],[4,5,6],[7,8,9]] --> 0`
- mirroring      `mirror [[1,2],[3,4]] --> [[1,3],[2,4]]`
- transform      `transform [1,2] [[1,2],[3,4]] --> [5,11]`
- multiplication `multiply [[1,2],[3,4]] [[5,6],[7,8]] --> [[19,22],[43,50]]`
- identity       `identity 3 --> [[1,0,0],[0,1,0],[0,0,1]]`
- addition       `addMatrix [[1,2],[3,4]] [[1,1],[1,1]] --> [[2,3],[4,5]]`
- subtraction    `subtractMatrix [[1,2],[3,4]] [[1,1],[1,1]] --> [[0,1],[2,3]]`
- inverse        `inverse [[1,2,3],[5,5,6],[8,8,9]] --> [[-1,2,-1],[1,-5,3],[0,2.7,-1.7]]`
- eigenvalues    `eigenvalues [[2,0,0],[1,2,1],[-1,0,1]] --> [2,1]`
