module Sequences where

import Data.Char (ord, chr)

maxOf2 :: Int -> Int -> Int
-- Returns first argument if it is larger than the second,
-- the second argument otherwise 
maxOf2 x y
  = if x > y then x else y

maxOf3 :: Int -> Int -> Int -> Int
-- Returns the largest of three Ints
maxOf3 a b c 
	= if a>b 
	  then (if a>c then a else c)
	  else (if b>c then b else c)

isADigit :: Char -> Bool
-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit c = (ord '0' <= ord c && ord c <= ord '9')


-- False otherwise
isAlpha :: Char -> Bool
-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
isAlpha c = (ord 'a' <= ord c && ord c <= ord 'z') || (ord 'A' <= ord c && ord c <= ord 'Z') 

digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt c 
	| ord c >= ord '0' && ord c <= ord '9' = ord c - 48
	| otherwise = 400 --400 is the natural error code for any incorrectly entered chars

toUpper :: Char -> Char
-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper c 
	| ord c <= ord 'z' && ord c >= ord 'a' = chr (ord c - 32)
	| otherwise = c

--
-- Sequences and series
--

-- Pre : all defined sequences below start with zero nth term

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n 
	= a + ( fromIntegral n )*d 

	-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n 
	 = a*(r^ (fromIntegral n))

-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d n
	 = ((fromIntegral n + 1 )/ 2) * (2*a + ((fromIntegral n)*d) ) 

-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r n 
	| r == 0  && n ==0 = a
	| r == 0 && n>0 = 0
	|otherwise = a*(r^(fromIntegral n + 1) - 1) / (r-1)