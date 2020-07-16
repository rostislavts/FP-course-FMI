{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}      -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}           -- use all your pattern matches!
--{-# OPTIONS_GHC -fwarn-missing-signatures #-}       -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}           -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}  -- no incomplete patterns in lambdas!


-- Task from week8

sumDigits :: Integer -> Integer
sumDigits 0 = 0
sumDigits n = lastDigit + sumDigits restDigits
  where lastDigit  = n `mod` 10
        restDigits = n `div` 10


-- Task 1        

type Point = (Double, Double)

dist' :: Point -> Point -> Double
dist' (x1, y1) (x2, y2) = sqrt (sqr dx + sqr dy)
  where dx    = x1 - x2
        dy    = y1 - y2
        sqr x = x * x


-- Task 2

area :: Point -> Point -> Point -> Double
area (x1, y1) (x2, y2) (x3, y3) = 
    sqrt (p * (p - a) * (p - b) * (p - c))
  where a = dist' (x1, y1) (x2, y2)
        b = dist' (x1, y1) (x3, y3)
        c = dist' (x2, y2) (x3, y3)
        p = (a + b + c) / 2


-- Task 3

(+*) :: Integer -> Integer -> Integer
x +* y = (x + y) * y


-- Task 4

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)



