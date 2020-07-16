{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-} -- use all your pattern matches!
--{-# OPTIONS_GHC -fwarn-missing-signatures #-} -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-} -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- no incomplete patterns in lambdas!


import Prelude hiding (sum, length, maximum, elem, 
                      reverse, take, drop, concat, 
                      zipWith, (<*>), map, filter, 
                      foldr, foldl, zip)



isFixpoint :: Eq a => (a -> a) -> a -> Bool
isFixpoint f x = x == f x

derive :: (Double -> Double) -> Double -> Double -> Double
derive f dx x = (f (x + dx) - f x) / dx

repeated :: ((a -> a) -> a -> a) -> Integer -> a -> a
repeated f n
    | n == 0 = id 
    | otherwise = f (repeated f (n - 1))


first :: (a, b, c) -> a
first (x,_,_) = x

second :: (a, b, c) -> b
second (_,b,_) = b

third :: (a, b, c) -> c
third (_,_,c) = c

type Point = (Double, Double)
type Triangle = (Point, Point, Point)
type Vector = Point

(<+>) :: Vector -> Vector -> Vector
(x, y) <+> (z, t) = (x + z, y + t)


(<*>) :: Vector -> Vector -> Vector
(x, y) <*> (z, t) = (x * z, y * t)


length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) 
  | p x       = x:filter p xs
  | otherwise = filter p xs


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv []      = nv
foldr op nv (x:xs) = op x (foldr op nv xs)

-- foldl op nv l
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ nv []      = nv
foldl op nv (x:xs) = foldl op (op nv x) xs


-- Task 1

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

-- Task 2

isSorted :: Ord a => [a] -> Bool
isSorted []         = True
isSorted [_]        = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted xs

-- Task 3

-- DONE 

-- Task 4

maximum :: Ord a => [a] -> a
maximum [x]    = x
maximum (x:xs) = max x (maximum xs)

-- Task 5

elem :: Eq a => a -> [a] -> Bool
elem _ []      = False
elem el (x:xs) = if x == el then True else elem el xs 

-- Task 6

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]


-- Task 7

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix (x:xs) (y:ys) = if x /= y then False else isPrefix xs ys


-- Task 8

take :: Integer -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs


-- Task 9

drop :: Int -> [a] -> [a]
drop 0 l = l
drop n (x:xs) 
  | n > length (x:xs) = [] 
  | otherwise         = drop (n - 1) xs


-- Task 10

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


-- Task 11

concat :: [[a]] -> [a]
concat [x] = x
concat (x:xs) = x ++ concat xs 


-- Task 12

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith op (x:xs) (y:ys) = op x y : zipWith op xs ys
