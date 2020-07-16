import Prelude hiding (enumFromTo)


enumFromTo :: Int -> Int -> [Int]
enumFromTo a b 
 | a > b = []
 | otherwise = a : enumFromTo (a + 1) b


repeat_ :: a -> [a]
repeat_ x = x : repeat_ x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

type Vector = (Int, Int)
addVectors :: Vector -> Vector -> Vector
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

ones :: [Int]
ones = 1 : ones

nats :: [Int]
nats = 0 : zipWith (+) ones nats

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

divides :: Int -> Int -> Bool
divides d n = n `rem` d == 0


sieve :: [Int] -> [Int]
sieve (x:xs) = x : sieve (filter (\y -> not (divides y x)) xs)

