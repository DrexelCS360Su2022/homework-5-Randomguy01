{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10 

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = if n > 0 then (toDigits (dropLastDigit n)) ++ [(n `mod` 10)] else []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOther' ((reverse xs), False))

doubleEveryOther' :: ([Integer], Bool) -> [Integer]
doubleEveryOther' ([], b) = []
doubleEveryOther' (xs, b) = (if b then [(head xs) * 2] else [head xs]) ++
  doubleEveryOther'(tail xs, not b)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = (sum (toDigits (head xs))) + (sumDigits (tail xs))

validate :: Integer -> Bool
validate n = lastDigit (sumDigits (doubleEveryOther (toDigits n))) == 0

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow f 0 = \x -> x
pow f 1 = f
pow f n = (f . (pow f (n - 1)))

g :: Integer -> Integer
g 0 = 0
g n = n - ((pow g 2) (n - 1))

h :: Integer -> Integer
h 0 = 0
h n = n - ((pow h 3) (n - 1))

d :: Int -> Integer -> Integer
d i 0 = 0
d i n = n - ((pow (d i) i) (n - 1))

--
-- Problem 3
--

powerSet s | isEmpty s = singleton empty
