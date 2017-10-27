# Grant's Linear Algebra Library

Linear algebra is useful for many things, so I'm making a library to better my understanding of it.

## Supported operations
### Gla.Vectors
- addition    `add [1,2] [3,4] --> [3,6]`
- subtraction `subtract [1,2] [3,4] --> [-2,-2]`
- dot product `dot [1,2] [3,4] --> 11`
- scaling     `scale 2 [3,4] --> [6,8]`

### Gla.Matrices
- determinant    `determinant [[1,2,3],[4,5,6],[7,8,9]] --> 0`
- mirroring      `mirror [[1,2],[3,4]] --> [[1,3],[2,4]]`
- multiplication `multiply [[1,2],[3,4]] [[5,6],[7,8]] --> [[19,22],[43,50]]`
- identity       `identity 3 --> [[1,0,0],[0,1,0],[0,0,1]]`
- inverse        `inverse [[1,2,3],[5,5,6],[8,8,9]] --> [[-1,2,-1],[1,-5,3],[0,2.7,-1.7]]`
