module KCTest (
  x, a, b, y, a', b'
) where

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