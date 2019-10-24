{-# LANGUAGE DataKinds #-}
module BlindEvaluations (
  cs, s, ws, ws', s', ar
) where

import Groups
import HomomorphicHidings

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
