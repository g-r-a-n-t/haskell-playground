module Gec.Types where

import Data.Char

-- Helpers --
hexCharValue c
  | x < 58 = x - 48
  | x < 71 = x - 55
  | otherwise = 0
  where x = ord (toUpper c)

asciiBits c = map (\b -> if b then 1 else 0) [x >= 8, x `mod` 8 >= 4, x `mod` 4 >= 2, x `mod` 2 == 1]
  where x = hexCharValue c

hexBits s = concat (map (\c -> asciiBits c) s')
  where s' = foldl (\acc c -> if elem c "0123456789ABCDEF" then acc ++ [c] else acc) [] s

hexValue s = foldl (\acc (b, i) -> if b == 1 then acc + 2^i else acc) 0 (zip (reverse (hexBits s)) [0..])

-- Curve = (a b p)
data Curve = Curve Integer Integer Integer deriving (Show)

-- Domain = (curve G n h)
data Domain = Domain Curve (Integer, Integer) Integer Integer deriving (Show)

secp256k1 = Domain curve (xg, yg) n h
  where p  = hexValue "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFC2F"
        n  = hexValue "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE BAAEDCE6 AF48A03B BFD25E8C D0364141"
        h  = 1
        a  = 0
        b  = 7
        xg = hexValue "79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798"
        yg = hexValue "483ADA77 26A3C465 5DA4FBFC 0E1108A8 FD17B448 A6855419 9C47D08F FB10D4B8"
        curve = Curve a b p
