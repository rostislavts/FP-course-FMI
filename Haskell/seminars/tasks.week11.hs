import Prelude hiding (sum, length, (++), (!!), maximum,
                        elem, reverse, take, drop, concat,
                        zip, zipWith, takeWhile, dropWhile)

-- Task 1
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

-- Task 2
length :: Num b => [a] -> b
length []     = 0
length (_:xs) = 1 + length xs

-- Task 3
(++) :: [a] -> [a] -> [a]
[]     ++ l = l
(x:xs) ++ l = x:xs ++ l

-- Task 4
(!!) :: (Eq b, Num b) => [a] -> b -> a
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n - 1)

-- Task 5
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Task 6
maximum :: Ord a => [a] -> a
maximum l = foldr1 max l

-- Task 7
elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
elem c (x:xs) = c == x || elem c xs

-- Task 8
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- Task 9
isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix _ [] = False
isPrefix [] _ = True
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- Task 10
take :: (Eq a,Num a) => a -> [b] -> [b]
take _ []     = []
take 0 (x:xs) = [x]
take n (x:xs) = x : take (n - 1) xs

-- Task 11
drop :: (Eq a,Num a) => a -> [b] -> [b]
drop _ []     = []
drop 0 l      = l
drop n (x:xs) = drop (n - 1) xs

-- Task 12
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys)  = (x,y) : zip xs ys

-- Task 13
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

-- Task 14
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith op (x:xs) (y:ys) = op x y : zipWith op xs ys

-- Task 15
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

-- Task 16
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) = if p x then dropWhile p xs else (x:xs)

-- Task 17
-- Helper Function
splitList :: Eq a => [a] -> [[a]]
splitList [] = []
splitList l  = if leftPart == [] then [] else leftPart : splitList rightPart 
  where leftPart = takeWhile (== (l !! 0)) l
        rightPart = dropWhile (== (l !! 0)) l 

maxRepeated :: (Eq a, Num b, Ord b) => [a] -> b
maxRepeated l  = maximum (map length splitedList)
  where splitedList = splitList l

-- Task 18
countOccurences :: (Eq a, Num b) => a -> [a] -> b
countOccurences _ [] = 0
countOccurences a (x:xs)
  | a == x    = 1 + countOccurences a xs
  | otherwise = countOccurences a xs

histogram :: (Eq a, Num b) => [a] -> [(a,b)]
histogram [] = []
histogram (x:xs) = (x, countOccurences x (x:xs)) : histogram filteredList
  where filteredList = filter (/=x) xs  

-- Task 19
--permutations :: [a] -> [[a]]


-- Task 20
--scanl, scanr