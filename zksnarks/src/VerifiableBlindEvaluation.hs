module VerifiableBlindEvaluation (
  a', x, b', _a', _x, _b'
) where

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

