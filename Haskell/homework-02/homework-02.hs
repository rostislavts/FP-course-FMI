-- Task 00
group :: Eq a => [a] -> [[a]]
{-
group [] = []
group (x:xs) = [x : equalX] ++ group rest
  where equalX = takeWhile (==x) xs
        rest   = dropWhile (==x) xs
-}
group = groupBy (==)

-- Task 01
quicksort :: (a -> a -> Ordering) -> [a] -> [a]
quicksort _ [] = []
quicksort comp (x:xs) = smallerSorted ++ [x] ++ biggerSorted
  where smallerSorted = quicksort comp [a | a <- xs, comp a x == LT]
        biggerSorted  = quicksort comp [a | a <- xs, comp a x /= LT]

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = quicksort

-- Task 02
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p (x:xs) = (x : equalX) : groupBy p rest
  where equalX = takeWhile (p x) xs
        rest   = dropWhile (p x) xs
    
-- Task 03.00
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

-- Task 03.01
(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)

-- Task 04
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map fst . sortBy (compare `on` snd) . map (id &&& f)

-- Task 05
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = map (map fst) . groupBy ((==) `on` snd) . map (id &&& f)

-- Task 06
classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f = groupOn f . sortOn f

-- Task 07
data NonEmpty a = a :| [a] 
  deriving (Show, Eq, Ord)

mapNonEmpty :: (a -> b) -> NonEmpty a -> NonEmpty b
mapNonEmpty f (x :| xs) = f x :| map f xs

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty = groupByNonEmpty (==)
    
groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty _ [] = []
groupByNonEmpty p (x:xs) = (x :| equalX) : groupByNonEmpty p rest
  where equalX = takeWhile (p x) xs
        rest   = dropWhile (p x) xs

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f = map (mapNonEmpty fst) . groupByNonEmpty ((==) `on` snd) . map (id &&& f)

classifyOnNonEmpty :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f = groupOnNonEmpty f . sortOn f

-- TESTS : 101 examples, 0 failures