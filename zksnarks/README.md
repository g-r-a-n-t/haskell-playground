# zk-snarks

This projects works through the 7 components laid out in this [article](https://z.cash/technology/zksnarks/) provided by z.cash. 

It's also used as an application for the [algebraic-structures](../algebraic-structures) project in this repo.

Running tests for the following code: `stack test`

## Section 1: "Homomorphic"Hidings

```haskell
-- Homomorphic Hiding
--
-- A homomorphic hiding is the same as a normal group homomorphism, except, the mapping function `E`
-- must be difficult to reverse i.e. given `x` you can compute `E(x)`, but given `E(x)` you can not compute `x`.
newHH _E _G _H = newHomomorphism _E _G _H

-- HH from Zp−2 to Zxp-1 where p = 11
--
-- Note that this is technically not a group homomorphism as Zp-2 is not a group since it does not include
-- the value 10 i.e. 1 + 9 = 10 /< Zp-2.
-- This value needed to be removed from the group since the order of Zp-1 is 10 since it does not include 0.
zpToZxpByGx = newHH _E _G _H
  where _E x = 2 ^ x -- g = 2
        _G   = newGroup [0..9] (+)
        _H   = newGroup [1..10] (*)
```

## Section 2: BlindEvaluations
```haskell
-- Note: The language in the second part is changed somewhat, where instead of having two groups (Zp and Zxp), we have
-- a single field (Fp). We'll still use the same homomorphism in this section for simplicity.

cs = [2,3,4] -- Alice's polynomial coefficients (2 + 3x + 4x^2)
s =  5       -- Bob's point (5 < Fp)

ws = (cs!!0 + (s * cs!!1) + ((s ^ 2) * cs!!2)) `mod` 11 -- Weighted sum (Neither Bob or Alice should know this value.)
ws' = (_E ws) `mod` 11                                  -- The result of applying E to the weighted sum.
  where Homomorphism _E _ _ = zpToZxpByGx

s' = map _E [1, s, s^2]                                 -- Applying E to Bob's point.
  where Homomorphism _E _ _ = zpToZxpByGx

ar = (foldl (\a (g, x) -> a * (g ^ x)) 1 (zip s' cs)) `mod` 11 -- Alice's result (should be equal to ws')
```

## Section 3: KCTest

```haskell
-- Bob's numbers
x = 6 -- x < Fxp (x used in place of alpha and Alice does not know this)
a = 3
b = x * a `mod` 11

-- Alice's numbers
-- She must respond with another x pair.
-- By "x pair" we mean, the second number must be the first value scaled by x.
-- Scaling both numbers provided by Bob with some value y will create another x pair.
y = 2
a' = y * a `mod` 11
b' = y * b `mod` 11

-- Bob not checks if x * a' = b'. This is something that only Bob can do since x is secret. At least I would assume.
-- See spec file.
```

## Section 4: Verifiable Blind Evaluations

```haskell
-- Extended KCA
as = [3,6]                -- Bob's a values (sent to Alice)
x = 4                     -- Bob's alpha value
bs = map (\a -> a * x) as -- Bob's b values (sent to Alice)

ys = [2, 5]                                   -- Alice's gamma values (not gamma anymore in the article, it uses c1 and c2)
a' = (ys!!0 * as!!0 + ys!!1 * as!!1) `mod` 11 -- Alice's a' value (shared with Bob)
b' = (ys!!0 * bs!!0 + ys!!1 * bs!!1) `mod` 11 -- Alice's b' value (shared with Bob)

-- Verifiable Blind Evaluation
_g = 2                                    -- Generator
_E _x = _g * _x                           -- Different homomorphic hiding from the last
_s = 3                                    -- Bob's seed
_x = 7                                    -- Bob's alpha value
_as = [_E 1, _E _s, _E ((_s^2) `mod` 11)] -- Bob's a values
_bs = map (\_a -> (_a * _x) `mod` 11) _as -- Bob's b values

_cs = [5, 3, 2] -- Alice's polynomial coefficients
_a' = (_cs!!0 * _as!!0 + _cs!!1 * _as!!1 + _cs!!2 * _as!!2) `mod` 11 -- Alice's a' value (shared with Bob)
_b' = (_cs!!0 * _bs!!0 + _cs!!1 * _bs!!1 + _cs!!2 * _bs!!2) `mod` 11 -- Alice's b' value (shared with Bob)

```