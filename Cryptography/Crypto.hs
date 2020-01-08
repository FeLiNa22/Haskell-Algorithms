module Crypto where

import Data.Char

import Prelude hiding (gcd)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

gcd :: Int -> Int -> Int
-- Pre : a > 0, b > 0
gcd a 0 = a
gcd a b = gcd b (mod a b)

phi :: Int -> Int
phi m
	| m == 1 	= 1
	| otherwise = length [x| x<-[0 .. m], gcd m x == 1]

-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
-- such that au + bv = d
computeCoeffs :: Int -> Int -> (Int, Int)
-- Pre :  a >= 0. b >= 0
computeCoeffs a 0 = (1, 0)
computeCoeffs a b = (v, u - q * v)
	where 
	(q,r) = quotRem a b
	(u,v) = computeCoeffs b r

-- Inverse of a modulo m
inverse :: Int -> Int -> Int
inverse a m 
	| gcd a m /= 1 		 = 0
	| (a*u) + (m*v) == 1 = u `mod` m
	where
	(u,v) = computeCoeffs a m   

-- Calculates (a^k mod m)
modPow :: Int -> Int -> Int -> Int
-- Pre : 0 <= a < m
modPow a k m
	| k <= 1 = (a^k) `mod` m
	| even k = modPow w e m `mod` m 
	| odd  k = a*(modPow w e m ) `mod` m 
    where
	w = (a^2) `mod` m
	e = k `div` 2
        
-- Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
-- Pre : x > 0
smallestCoPrimeOf x = smallestCoPrimeOf' x 2
	where smallestCoPrimeOf' a b
		| gcd a b == 1  = b
		| otherwise 	= smallestCoPrimeOf' a (b+1)

-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q = ((e, n), (d,n))
	where 
	e 	= smallestCoPrimeOf def 
	d 	= inverse e def 
	n 	= p * q
	def = (p - 1) * (q - 1)

-- RSA encryption/decryption
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt x (e,n) = modPow x e n	

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d,n) = modPow c d n

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt c = ord c - ord 'a'

-- Returns the n^th letter
toChar :: Int -> Char
toChar n = chr (n + ord 'a')

-- "adds" two letters
add :: Char -> Char -> Char
add c1 c2 = toChar ((posc1 + posc2) `mod` 26)
		where
		posc1 = toInt c1
		posc2 = toInt c2

-- "substracts" two letters
substract :: Char -> Char -> Char
substract c1 c2 = toChar ((posc1 - posc2) `mod` 26)
		where
		posc1 = toInt c1
		posc2 = toInt c2

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt k [] 	= []
ecbEncrypt k (x:xs) = (add x k) : ecbEncrypt k xs

ecbDecrypt :: Char -> String -> String
ecbDecrypt k [] 	= []
ecbDecrypt k (x:xs) = (substract x k) : ecbDecrypt k xs

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
--
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt k v [] 		= []
cbcEncrypt k v (x:str) 	=  c : cbcEncrypt k c str
		where 
		a = add x v
		c = add a k

cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt k v [] 		= []
cbcDecrypt k v (c:str) 	=  x : cbcDecrypt k c str
		where 
		a = substract c k
		x = substract a v
