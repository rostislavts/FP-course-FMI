{-# LANGUAGE InstanceSigs #-} -- allows us to write signatures in instance declarations

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

import Prelude hiding (Maybe(..), maybe, lookup, find)

-- типов псевдоним (type synonym), type alias.
--type Student'' = (String, String, Int)
              -- (име, специалност, фн)
--  deriving Show

-- За да достъпим информацията за студент
-- се налага да направим функции, които изглеждат така:
--getName'' :: Student'' -> String
--getName'' (x,_,_) = x
-- досадно е да правим това за всяко поле от типа


-- Може да реализираме тип и така
--data Student' = MkStudent' String String Int
-- обаче това има същия проблем
--getName' :: Student' -> String
--getName' (MkStudent' x _ _) = x

-- Хаскел има синтактично захар за дефиниране на такива getter-и.
-- Това е т.нар. record syntax.
data Student
  = MkStudent { getName :: String
              , getCourse :: String
              , getFacultyNumber :: Int
              }
  deriving (Show, Eq)
-- Сега Student е същия тип като Student', освен:
-- * създадени са автоматично функции getName, getCourse, getFacultyNumber
--   за типа Student
-- * Автоматично генерираната инстанция на Show за Student
--   е различна от тази за Student'

-- Може да има стойност, може да няма
data Maybe a
  = Nothing
  | Just a
  deriving Show
-- Допълнителна информация към оригиналната стойност.
-- Всеки обект можем да го обгърнем в Just:
--
-- Just 5
-- Just [1,2,3,4]
--
-- Но допълнително може и да липсва стойността - Nothing
-- Nothing е полиморфична константа
-- ghci> :t Nothing
-- Nothing :: Maybe a

-- Задача: равенство на Maybe
-- Повдигаме равенството на `а` върху `Maybe a`
instance (Eq a) => Eq (Maybe a) where
  (==) :: (Eq a) => Maybe a -> Maybe a -> Bool
  Nothing == Nothing = True
  Just a  == Just b  = a == b
  _       == _       = False 

-- Задача: разглабяне на списък
--
-- safeUncons [] == Nothing
-- safeUncons [10,13,12] == Just (10, [13,12])
safeUncons :: [a] -> Maybe (a, [a])
safeUncons []     = Nothing
safeUncons (x:xs) = Just (x, xs)

-- Задача: целичислено делене
-- Не може да делим на 0, затова невинаги връщаме резултат.
-- Връщаме цяла част на делението и остатък.
safeDiv :: Int -> Int -> Maybe (Int, Int)
safeDiv _ 0 = Nothing
safeDiv n d = Just (n `div` d, n `mod` d)

-- Задача: Търсим първата стойност в списък, която удовлетворява предикат.
-- Връщаме Nothing ако не намерим такава.
--
-- find even [1,3,5,6,7] == Just 6
-- find even [1,3,5,7,9] == Nothing
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs)
  | p x       = Just x
  | otherwise = find p xs

-- Задача: Премахване на префикс от низ
-- Но втория низ не е задължително да съдържа първия,
-- затова искаме да можем да върнем "грешка".
--
-- stripPrefix "foo" "foobar" == Just "bar"
-- stripPrefix "mgla" "kpop" == Nothing
stripPrefix :: String -> String -> Maybe String
stripPrefix str1 str2 = if str1 `isPrefOf` str2 then cutStr str1 str2 else Nothing
  where isPrefOf []     _      = True
        isPrefOf (x:xs) (y:ys) = x == y && isPrefOf xs ys
        cutStr str1 str2 = Just $ drop (length str1) str2


-- Задача: Търсене в асоциативен списък, `assoc` в Scheme
--
-- lookup 5 [(10, 'a'), (5,'c')] == Just 'c'
-- lookup 13 [(10, 'a'), (5,'c')] == Nothing
lookup :: Eq k => k -> [(k, v)] -> Maybe v
lookup _ []       = Nothing
lookup k (kv:kvs) = if k == fst kv then Just $ snd kv else lookup k kvs


-- Задача: прилага функция към стойност, ако има такава
-- Това е различно нещо от Data.Maybe.mapMaybe
--
-- mapMaybe succ Nothing == Nothing
-- mapMaybe succ (Just 41) == Just 42
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just a) = Just $ f a

-- Тип двоично дърво, който по-надолу (за краткота) ще наричам просто дърво.
data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

-- Задача: равенство на дървета
instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty  == Empty  = True
  _      == Empty  = False
  Empty  == _      = False
  Node a leftA rightA == Node b leftB rightB = a == b && leftA == leftB && rightA == rightB


-- Задача: сбор на всички числа в дърво
--
-- sumTree (Node 5 (Node 2 Empty Empty) (Node 7 Empty Empty)) == 14
sumTree :: Num a => Tree a -> a
sumTree Empty               = 0
sumTree (Node a left right) = a + sumTree left + sumTree right 

-- Има разлика как ще обхождаме дървото.
-- В случая за `treeToList` е близко до представите ни да обхождаме "ляво-корен-дясно":
--
-- treeToList (Node 5 (Node 2 Empty Empty)
--                    (Node 7 (Node 6 Empty Empty)
--                            Empty))
--  == [2,5,6,7]

-- Задача: "сгъване" на дърво във списък
--
-- treeToList (Node 5 (Node 2 Empty Empty) (Node 7 Empty Empty)) == [2,5,7]
treeToList :: Tree a -> [a]
treeToList Empty               = []
treeToList (Node a left right) = treeToList left ++ [a] ++ treeToList right

-- Задача: добавяне на елемент в дърво, следвайки наредбата.
-- Разсъждаваме на следния принцип:
--    във всеки един момент сравняваме първия аргумент с корена на дървото.
--    Ако аргументът е по-голям от корена, тръгваме надясно.
--    Иначе тръгваме наляво.
--
-- Ако двоичното дърво е наредено, то тази функция ще запази наредеността.
--
-- insertOrdered 5 Empty == Node 5 Empty Empty
-- insertOrdered 5 (Node 10 Empty Empty) == Node 10 (Node 5 Empty Empty) Empty
-- insertOrdered 5 (Node 3 Empty Empty) == Node 3 Empty (Node 5 Empty Empty)
insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered x Empty               = (Node x Empty Empty)
insertOrdered x (Node a left right) = if x > a then (Node a left (insertOrdered x right)) else (Node a (insertOrdered x left) right)
 
-- Задача: създаване на двоично наредено дърво по даден списък.
-- Може дървото ви да изглежда по различен начин от примерите.
--
-- listToTree [1..10] == Node 10 (Node 9 (Node 8 (Node 7 (Node 6 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty
-- listToTree [1,10,2,9,3,8] == Node 8 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) Empty) (Node 9 Empty (Node 10 Empty Empty))
listToTree :: Ord a => [a] -> Tree a
listToTree l = foldr insertOrdered Empty l

-- Задача: сортиране на списък чрез двоично наредено дърво.
bstSort :: Ord a => [a] -> [a]
bstSort = treeToList . listToTree

-- Задача: map за дървета,
-- заменя стойностите в дърво спрямо дадена функция
--
-- mapTree (+4) (Node 10 (Node 5 Empty Empty) Empty) = Node 14 (Node 9 Empty Empty) Empty
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty               = Empty
mapTree f (Node a left right) = (Node (f a) (mapTree f left) (mapTree f right))

-- Задача: проверка дали всички стойности в дървото изпълняват предикат
--
-- allTree odd (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) == True
-- allTree (<7) (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) == False
allTree :: (a -> Bool) -> Tree a -> Bool
allTree _ Empty               = True
allTree p (Node a left right) = p a && allTree p left && allTree p right     

-- Задача: принадлежност в дърво
--
-- elemTree 5 (listToTree [1..10]) == True
-- elemTree 42 (listToTree [1..10]) == False
elemTree :: Eq a => a -> Tree a -> Bool
elemTree _ Empty               = False
elemTree x (Node a left right) = x == a || elemTree x left || elemTree x right 

-- Задача: търсене в дърво
-- Намира елемент на дървото, който удовлетворява предиката.
--
-- findTree even (listToTree [1..10]) == Just 2 -- или Just 4 или Just 6 или Just 8 или Just 10
-- findTree (>20) (listToTree [1..10]) == Nothing
findTree :: (a -> Bool) -> Tree a -> Maybe a
findTree p tree =  if null result then Nothing else Just (head result) 
    where result = filter p $ treeToList tree 


-- Foldable и Traversable
-- wiki.haskell.org/Typeclassopedia


{-
-- Останалите задачи от практикума:

-- EXERCISE: Fallback
--
-- EXAMPLES:
-- fromMaybe 5 Nothing == 5
-- fromMaybe 5 (Just 10) == 10
fromMaybe :: a -> Maybe a -> a
fromMaybe = undefined

-- EXERCISE: Total deconstruction of a Maybe
-- Sometimes useful instead of pattern matching
-- or with higher order functions
--
-- EXAMPLES:
-- maybe 5 succ (Just 10) == 11
-- maybe 5 succ Nothing == 5
maybe :: b -> (a -> b) -> Maybe a -> b
maybe = undefined

-- EXERCISE: Convert a maybe to a list
maybeToList :: Maybe a -> [a]
maybeToList = undefined

-- EXERCISE: Keep the first element of a list, if there are any
listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

-- EXERCISE: Sum all the values inside the maybe
--
-- EXAMPLES:
-- sumMaybe Nothing == 0
-- sumMaybe (Just 5) == 5
sumMaybe :: Num a => Maybe a -> a
sumMaybe = undefined

-- EXERCISE: Folding a Maybe
--
-- EXAMPLES:
-- foldMaybe (Just [1,2,3]) == [1,2,3]
-- foldMaybe (Nothing :: Maybe [Int]) == []
foldMaybe :: Monoid a => Maybe a -> a
foldMaybe = undefined

-- EXERCISE: Analogue of foldMap for lists
foldMapMaybe :: Monoid b => (a -> b) -> Maybe a -> b
foldMapMaybe = undefined

-- EXERCISE: Get all the Justs from a list
--
-- EXAMPLES:
-- catMaybes [Just 5, Nothing, Just 10] == [5, 10]
catMaybes :: [Maybe a] -> [a]
catMaybes = undefined

-- EXERCISE : Map all the values of a list with possible failure
-- and get back all the successful results
--
-- This is what Data.Maybe.mapMaybe does.
-- Contrast this with filter!
--
-- mapListMaybe safeUncons [[], [1,2], [], [2,3]] == [(1, [2]), (2, [3])]
mapListMaybe :: (a -> Maybe b) -> [a] -> [b]
mapListMaybe = undefined

-- EXERCISE: "Adapter" to convert (a -> Bool) functions to Maybe returning ones
--
-- EXAMPLES:
-- (onBool even) 3 == Nothing
-- (onBool even) 4 == Just 4
onBool :: (a -> Bool) -> a -> Maybe a
onBool = undefined

-- EXERCISE: Conditional execution
--
-- (note how this looks like flip concatMap :: [a] -> (a -> [b]) -> [b] !)
--
-- The idea is that, if at any point one of our computations returns a Nothing,
-- we fail the whole thing early.
--
-- EXAMPLES:
-- xs == [(2, "k"), (3, "e"), (42, "k")]
-- x = lookup 2 xs `ifJust` (\x ->
--     lookup 3 xs `ifJust` (\y ->
--     lookup 42 xs `ifJust` (\z ->
--     Just $ x ++ y ++ z)))
-- x == Just "kek"
--
-- xs == [(13, "k"), (3, "e"), (42, "k")]
-- x = lookup 2 xs `ifJust` (\x ->
--     lookup 3 xs `ifJust` (\y ->
--     lookup 42 xs `ifJust` (\z ->
--     Just $ x ++ y ++ z)))
-- x == Nothing
ifJust :: (Maybe a) -> (a -> Maybe b) -> Maybe b
ifJust = undefined

-- Induced Semigroup by a
instance Semigroup a => Semigroup (Maybe a) where
  (<>) :: Maybe a -> Maybe a -> Maybe a
  (<>) = undefined

-- Maybe lifts any Semigroup to a Monoid!
instance Semigroup a => Monoid (Maybe a) where
  mempty :: Maybe a
  mempty = undefined



-- EXERCISE: Fold a tree
foldTree :: Monoid a => Tree a -> a
foldTree = undefined

-- EXERCISE: ..while first mapping
foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree = undefined

-- EXERCISE: Find all that satisfy a "predicate"
-- Recursively find all that return a Just
-- ifJust might be useful here
findAll :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
findAll = undefined
-}