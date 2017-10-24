# Grant's Linear Algebra Library

Linear algebra is useful for many things, so I'm making a library to better my understanding of it.

## Supported operations
### Vectors
- addition    `Gla.Vectors.add [1,2] [3,4] --> [3,6]`
- subtraction `Gla.Vectors.subtract [1,2] [3,4] --> [-2,-2]`
- dot product `Gla.Vectors.dot [1,2] [3,4] --> 11`
- scaling     `Gla.Vectors.scale 2 [3,4] --> [6,8]`

### Matrices
- determinant    `Gla.Matrices.determinant [[1,2,3],[4,5,6],[7,8,9]] --> 0`
- mirroring      `Gla.Matrices.mirror [[1,2],[3,4]] --> [[1,3],[2,4]]`
- multiplication `Gla.Matrices.multiply [[1,2],[3,4]] [[5,6],[7,8]] --> [[19,22],[43,50]]`
- identity       `Gla.Matrices.identity 3 --> [[1,0,0],[0,1,0],[0,0,1]]`
