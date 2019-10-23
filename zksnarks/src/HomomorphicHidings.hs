{-# LANGUAGE DataKinds #-}
module HomomorphicHidings (
  newHH,
  zpToZxpByGx
) where

import Groups

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

