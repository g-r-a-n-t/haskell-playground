module ComputationsToPolynomials (

) where

-- An arithmetic circuit consists of a binary tree of operations working on two values of a single type.
-- This is a flattened tree where:
-- lchild = i * 2 + 1
-- rchild = i * 2 + 2
-- parent = (i + 1) / 2 - 1 (this is integer math)

-- Example:
--                    x (i=0)
--        x (i=1)                + (i=2)
--  + (i=3)   x (i=4)       + (i=5)    x (i=6)
data Circuit = Circuit [a -> a -> a]