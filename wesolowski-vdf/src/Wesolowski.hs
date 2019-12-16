{-# LANGUAGE DataKinds #-}
module Wesolowski where

import Codec.Crypto.RSA.Pure as RSA
import Crypto.Random as RND
import Data.ByteString.Char8 as BS
import Crypto.Random.DRBG as DRBG
import Control.Monad

-- Generate a random keypair(pk, sk) of the given modulus bits(s).
-- keygen(s) -> (pk, sk)
keygen :: CryptoRandomGen g => g -> Int -> Either String (Integer, Integer)
keygen g s =
  case RSA.generatePQ g s of
    Left _ -> Left "Unable to generate RSA keypair"
    Right (p, q, _) -> Right (p*q, (p-1)*(q-1))

-- Compute a value(y) from input(x) with a specified number of sequential
-- steps(t) using the secret key(sk) and provide a proof(p).
-- trapdoor(sk, x, t) -> (y, p)
trapdoor :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
trapdoor pk sk x t =
  let g = x -- TODO: use a hash function
      e = modExp 2 t sk -- 2^t mod |G|
      y = modExp g e pk -- g^e mod G
      l = toPrime $ (show g) ++ "-" ++ (show y) -- Hprime(bin(g)|||bin(y)) TODO: implement as specified
      r = modExp 2 t l -- least residue of 2^t mod l
      --q =  -- (2^t - r)l^-1 mod |G|
  --     p = -- g^q
  -- in (y, p)
  in (y, 0)


-- Compute a value(y) from input(x) with a specified number of sequential
-- steps(t) using the pubic key(pk) and provide a proof(p).
-- eval(pk, x, t) -> (y, p)
eval :: Integer -> Integer -> Integer -> (Integer, Integer)
eval pk x t =
  let g = x
      y = modExp g (2^t) pk
  in (y, 0)

-- Verifies that either eval or trapdoor have been computed correctly given
-- the inputs and outputs.
-- verify(pk, x, y, p, t) -> valid
verify :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
verify pk x y p t = False

-- Modular exponentiation. This is fast, but not memory safe, which makes
-- it impractical. Needs to be updated.
modExp :: Integer -> Integer -> Integer -> Integer
modExp x y n = do
  i <- 0
  when (i < y) $ do
    i = i + 1


-- Hashes a given string to some prime.
toPrime :: String -> Integer
toPrime s =
  case RND.newGen (BS.pack (s++(Prelude.unwords $ Prelude.replicate 40 "0"))) of
      Right g ->
        case RSA.largeRandomPrime (g :: DRBG.HashDRBG) 16 of
          Right (p, _) -> p
          Left _ -> 0
      Left e -> 0

-- Extended Euclidean Algorithm
-- taken from https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm#Extended_2
eGCD :: Integer -> Integer -> (Integer,Integer,Integer)
eGCD 0 b = (b, 0, 1)
eGCD a b = let (g, s, t) = eGCD (b `mod` a) a
           in (g, t - (b `div` a) * s, s)

-- Modular inverse
-- ax = 1 (mod n)
modInv :: Integer -> Integer -> Integer
modInv a n
 | x*a' + y*n == 1 && gcd == 1 = x `mod` n
 | otherwise = error "unable to compute inverse"
 where a' = a `mod` n
       (gcd, x, y) = eGCD a' n
