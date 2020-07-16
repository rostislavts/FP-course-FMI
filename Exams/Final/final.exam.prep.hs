
-- 2016

-- Var A

-- Task1

getFirstCol :: [[a]] -> [a]
getFirstCol = map head

delFirstCol :: [[a]] -> [[a]]
delFirstCol = map tail

transpose :: Eq a => [[a]] -> [[a]]
transpose [[]] = [[]]
transpose m     
  | head m == [] = []
  | otherwise    = (getFirstCol m) : (transpose $ delFirstCol m) 

predicate :: Eq a => [a] -> [[a]] -> Bool
predicate col m = any (\row -> all (\el -> elem el row) col) m

findColumns :: Eq a => [[a]] -> Int
findColumns m = length $ filter (\col -> predicate col m) mTransposed
  where mTransposed = transpose m

-- Task2

combine :: Eq a => (Int -> a) -> (Int -> a) -> (a -> a -> a) -> Int -> a
combine f g h x = h (f x) (g x)

check :: Eq a => Int -> Int -> [(Int -> a)] -> [(a -> a -> a)] -> Bool
check a b uns bins = 
    any (\f -> 
        any (\g -> 
            any (\h ->
                any (\t ->
                    all (\k -> 
                        (combine f g h k) == (t k)) [a..b]) uns) bins) uns) uns
            

-- Task3

type Plant = (String, Int, Int)

generateDegreeList :: [Plant] -> [Int]
generateDegreeList [] = []
generateDegreeList ((name,min,max):xs) = min : max : generateDegreeList xs

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups (filter (/=x) xs)

generatePairs :: [Int] -> [(Int, Int)]
generatePairs []     = []
generatePairs (x:xs) = [ (x, el) | el <- xs ] ++ generatePairs xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort greater
  where smaller = (filter (<=x) xs)
        greater = (filter (>x) xs)

getPlantsByDegrees :: [Plant] -> (Int, Int) -> [String]
getPlantsByDegrees [] _ = []
getPlantsByDegrees ((name, currMin, currMax) : xs) (min, max)
  | currMin <= min && currMax >= max = name : getPlantsByDegrees xs (min, max)
  | otherwise                        = getPlantsByDegrees xs (min, max)

garden :: [Plant] -> [((Int, Int), [String])]
garden plantList = filter (\(x, l) -> (length l) == maxElements) plantsByDegrees
    where plantsByDegrees = map (\pair -> (pair, getPlantsByDegrees plantList pair)) pairList
          pairList = generatePairs $ quicksort $ removeDups $ generateDegreeList plantList
          maxElements = maximum $ map length $ map (\(x,l) -> l) plantsByDegrees

-- Task4
children :: Eq a => a -> [[a]] -> [a]
children u (l:ls)
  | u == head l = tail l
  | otherwise   = children u ls

vertices :: [[a]] -> [a]
vertices = map head

extendPath :: Eq a => [a] -> [[a]] -> [[a]]
extendPath path g = 
    let current = head path 
    in [ v : path | v <- children current g ]

acyclic :: Eq a => [a] -> Bool
acyclic (x:xs) = not $ elem x xs

bfs :: Eq a => [[a]] -> [[a]] -> [[a]]
bfs paths g = 
    if (length extendedPaths) == (length paths) 
          then map reverse paths
          else bfs extendedPaths g
  where extendedPaths = filter acyclic $ removeDups $ paths ++ (concat (map (\path -> extendPath path g) paths))

getPathWithLen :: Eq a => [[a]] -> Int -> [a]
getPathWithLen (p:ps) len
  | length p == len = p
  | otherwise       = getPathWithLen ps len

maxPath :: Eq a => [[a]] -> a -> [a]
maxPath g u = getPathWithLen paths maxLength
  where paths = bfs [[u]] g
        maxLength = maximum $ map length paths


-- Var B

-- Task1

hasColumn m =
    any (\col -> 
        all (\num -> 
            all (\row -> elem num row) m) col) mTransposed
  where mTransposed = transpose m            

-- Task2 

combine2 f g h x = f x (g x (h x))

check2 a b uns bins = 
    any (\f -> 
        any (\g -> 
            any (\h -> 
                any (\t -> 
                    all (\k -> (combine2 f g h k) == (t k)) [a..b]) uns) uns) bins) bins

-- Task3

type Show = (String, Int, Int)

getName (n, _, _) = n

getStartH (_, h, _) = h

getDuration (_, _, d) = d

getFinalTime show = getStartH show + getDuration show / 60

getIntervalHours showList = map (\el -> (el, el + 1)) [(minimum allStartH)..(maximum allStartH)]
    where allStartH = map getStartH showList

getShowsByInterval showList (start, end) = 
    map getName $ filter (\(_, s, e) -> (start <= s && s < end) || (s < start && start < e)) $
     map (\s@(n, sH, d) -> (n, sH, (getFinalTime s))) showList


getCommonDuration showNames showList (start, end) =
    minimum $ map (\(_,s,d) -> d - (start - s) * 60 ) $ filter (\(n,_,_) -> elem n showNames) showList


showtime showList =  head $ map (\((s,e),l) -> ((round s, round e), l) ) $ filter (\(_,l) -> (length l) == maxProduction) resultList
  where intervals = getIntervalHours showList
        resultList = map (\interval@(s,e) -> ((s , getCommonDuration (getShowsByInterval showList interval) showList interval), getShowsByInterval showList interval)) intervals
        maxProduction = maximum $ map length $ map (\(_,list) -> list) resultList 

-- Task4 
 
maxCycle g u = head $ filter (\path -> (length path) == maxLength) cyclePaths
  where paths = map reverse $ bfs [[u]] g
        cyclePaths = filter (not . acyclic) $ map reverse $ removeDups $ paths ++ (concat (map (\path -> extendPath path g) paths))
        maxLength = maximum $ map length cyclePaths


-- 2017

-- Var A

-- Task1

getSubIntervals a b
    | a > b = []
    | otherwise = [ (a,x) | x <- [a+1 .. b] ] ++ getSubIntervals (a+1) b

largestInterval f g a b = head $ filter (\(a,b) -> b - a == maxIntervalLen) goodIntervals
  where subIntervals = getSubIntervals a b
        goodIntervals = filter (\(a,b) -> all (\k -> f k == g k) [a..b]) subIntervals
        maxIntervalLen = maximum $ map (\(a,b) -> b - a) goodIntervals


-- Task2 

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Prelude.Show, Ord, Eq)

findMinEl Empty = 2^31 - 1
findMinEl (Node a left right) = min a (min (findMinEl left)  (findMinEl right))

findMaxEl Empty = -(2^31)
findMaxEl (Node a left right) = max a (max (findMaxEl left)  (findMaxEl right))

intervalTree Empty = Empty
intervalTree t@(Node a left right) = 
    (Node (findMinEl t, findMaxEl t) 
      (intervalTree left) 
      (intervalTree right))

-- Task3

nats = [0..]

sumOfSquares = [ n^2 + k^2 | n <- nats, k <- [0..n] ]

-- Task4 

type Video = (String, Int)

average list = sum list `div` length list

averageDuration =  average . map snd 
 
averageVideo videoList = fst $ head $ filter (\(_,d) -> avDur - d == closestDist) videoList
  where avDur = averageDuration videoList
        closestDist = minimum $ filter (>= 0) $ map (\d -> avDur - d)  $ map snd videoList

-- Var B

-- Task1

subIntervals a b
  | a > b = []
  | otherwise = [ (a,k) | k <- [a..b] ] ++ subIntervals (a+1) b 

countIntervals f g a b = 
    length $ filter (\(k,m) -> 
        all (\x -> f x /= g x) [k..m]) subInts 
  where subInts = subIntervals a b


-- Task2

-- Like var A

-- Task3

sumsOfCubes = [ n^3 + k^3 | n <- nats, k <- [0..n] ] 

-- Task4 

type Shoes = (String, Int)

groupByModel []     = []
groupByModel (x:xs) = 
    map removeDups $ [ k | k <- (x:xs), fst k == fst x ] : groupByModel rest
  where rest = filter (\el -> fst el /= fst x) xs

bestRange shoesList = fst $ head $ head $ filter (\l -> length l == maxLen) grouped
  where grouped = groupByModel shoesList
        maxLen = maximum $ map length grouped

-- 2018

-- Var A

-- Task1 

-- Problem ???
generateExponents k l = [ x^k * y^l | y <- [1..], x <- [1..y] ]

-- Task2








