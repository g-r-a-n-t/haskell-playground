# Algebraic Structures

This library supplies types for common algebraic structures and provides functions that check the validity of them.

Running tests:
```sh
> stack test
```

Sample usage:
```haskell
import Math.Algebra.Group.PermutationGroup
import AlgebraicStructures.Groups

-- Create some groups 

-- read: https://en.wikipedia.org/wiki/Permutation_group#Examples
permuFourGroup = newGroup _S (*)
  where _S    = [e, a, b, a * b]
        a     = p [[1,2],[3],[4]]
        b     = p [[1],[2],[3,4]]
        e     = 1

-- read: https://en.wikipedia.org/wiki/Klein_four-group
kleinFourGroup = newGroup _S op
  where _S = ["e", "a", "b", "ab"]
        op a b
          | a == "e" = b -- Product of an element an identity
          | b == "e" = a
          | a == b = "e" -- Inverse of an element is itself
          | a == "a" && b == "b" = "ab" -- Product of "a" and "b" is "ab"
          | a == "b" && b == "a" = "ab"
          | a == "a" || b == "a" = "b" -- At this point we know one of the elements is "ab"
          | a == "b" || b == "b" = "a"

-- Define an isomorphism

-- These two groups are isomorphic over a direct map
k4ToM4byMap = newIsomorphism f kleinFourGroup permuFourGroup
  where f x
          | x == "e" = 1
          | x == "a" = p [[1,2],[3],[4]]
          | x == "b" = p [[1],[2],[3,4]]
          | x == "ab" = (f "a") * (f "b")

-- Inspect the structure of the groups and isomorphism!

isGroup permuFourGroup -- (True, "") note: also abelian 
isAbelianGroup kleinFourGroup -- (True, "") 
isIsomorphic k4ToM4byMap -- (True, "")
```