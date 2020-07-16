{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}      -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}           -- use all your pattern matches!
--{-# OPTIONS_GHC -fwarn-missing-signatures #-}       -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}           -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}  -- no incomplete patterns in lambdas!


-- Task 1

plusTen :: Int -> Int
plusTen = (+10)


-- Task 2

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)


fact' :: Integer -> Integer
fact' n 
  | n == 0    = 1
  | otherwise = n * fact' (n - 1)

-- We can also do it with if then else construction

-- Task 3

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


-- Task 4

gcd' :: Integer -> Integer -> Integer
gcd' _ 0 = 0
gcd' 0 _ = 0
gcd' a b 
  | a == b    = a
  | a > b     = gcd' (a - b) b
  | otherwise = gcd' a (b - a) 


-- Task 5

lcm' :: Integer -> Integer -> Integer
lcm' a b = (a * b) `div` gcd' a b


sumDigits :: Integer -> Integer
sumDigits n
  | n `div` 10 == 0 = n
  | otherwise       = lastDigit + sumDigits (n `div` 10)
  where lastDigit   = n `mod` 10


