module Gvdf.Wesolowski where

-- Generate a keypair(pk, sk) based on some seed(s).
-- keygen(s) -> (pk, sk)
keygen :: Integer ->  (Integer, Integer)
keygen s = (0, 0)

-- Compute a value(y) from input(x) with a specified number of sequential
-- steps(t) using the secret key(sk) and provide a proof(p).
-- trapdoor(sk, x, t) -> (y, p)
trapdoor :: Integer -> Integer -> Integer -> (Integer, Integer)
trapdoor sk x t = (0, 0)

-- Compute a value(y) from input(x) with a specified number of sequential
-- steps(t) using the private key(pk) and provide a proof(p).
-- eval(pk, x, t) -> (y, p)
eval :: Integer -> Integer -> (Integer, Integer)
eval pk x t = (0, 0)

-- Verifies that either eval or trapdoor have been computed correctly given
-- the inputs and outputs.
-- verify(pk, x, y, p, t) -> valid
verify :: Integer -> Integer -> Integer -> Integer -> Integer -> Boolean
verify pk x y p t = False
