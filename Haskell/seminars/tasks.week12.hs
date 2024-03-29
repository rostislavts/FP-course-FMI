{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- no incomplete patterns in lambdas!
{-# LANGUAGE InstanceSigs #-}                      -- allows us to write signatures in instance declarations

module Class_12 where

import Prelude hiding (flip, (.), ($), elem)

-- Pattern matching където си поискаме
fib :: Int -> Int
fib n = case n of
          0 -> 1
          1 -> 1
          m -> fib (m - 1) + fib (m - 2)

-- Всъщност pattern matching-а е синтактична захар за case

isPrime :: Int -> Bool
isPrime n = foldr (\x r -> (notDivides n x) && r) True [2..maxPossibleDivisor]
  where notDivides n x = n `mod` x /= 0
        maxPossibleDivisor = n `div` 2
        
--- Няколко полезни оператора/функции


flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- Прилагане на фукнция.
($) :: (a -> b) -> a -> b
f $ x = f x

-- с най-нисък приоритет (тоест ще се изпълни последнo) и дясно асоциативно.
infixr 0 $

-- Направете сравнение със стандартното прилагане на функции в Haskell,
-- което е ляво асоциативно и с най-висок приoритет.
--infixl 9 #

-- Пример: Искаме да вземем първите n прости числа на квадрат
lll n = take n (map (^2) (filter isPrime [2..]))
sss n = take n $ map (^2) $ filter isPrime [2..]

-- Спомняме си композицията
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

mmm n = take n . map (^2) . filter isPrime $ [2..]

-- Пример: отрицание на предикат
complement :: (a -> Bool) -> (a -> Bool)
complement p = (\x -> not $ p x)
--complement p x = not $ p x
--complement p = not . p
--complement = (not .)


--- Type Classes

-- Нека разгледаме първо някой полиморфични функции над списъци
-- reverse :: [a] -> [a]
-- head :: [a] -> a
-- length :: [a] -> [a]
-- Тези функции могат да работят с елементи от произволен тип
-- Още повече те не се интересуват от самите елементи
-- Това се нарича параметричен полиморфизъм
-- ([] е полиморфна константа)

elem :: Int -> [Int] -> Bool
elem _ []     = False
elem y (x:xs) = y == x || y `elem` xs

-- Празният списък е полиморфна константа
-- Числата са полиморфни константи

-- Инстанция на типов клас наричаме всеки тип,
-- за който са реализирани операциите зададени в класа.
-- Тези операции наричаме методи на съответния клас.
-- Нека видим как можем да направим някой тип инстанция на някой клас.

data Bool' = False' | True'

data Parity = Even | Odd

--data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

-- Нека разгледаме няколко основни класа - Num, Eq, Ord
-- И ще видим как да направим Parity тяхна инстанция

-- Eq
-- Достатъчно е да се имплементира или (==), или (/=)
--class Eq a where
--  (==) :: a -> a -> Bool
--  (==) = not . (/=)
--  (/=) :: a -> a -> Bool
--  (/=) = not . (==)

instance Eq Parity where
  (==) :: Parity -> Parity -> Bool
  Odd  == Odd  = True
  Even == Even = True
  _    == _    = False
  (/=) :: Parity -> Parity -> Bool
  Even /= Odd  = True
  Odd  /= Even = True
  _    /= _    = False

-- Закони за Eq (еквивалентност):
-- За всяко x y z:
--   x == x
--   x == y -> y == x
--   x == y && y == z -> x == z



-- Ord
-- Достатъчно е да се имплементира едно от: compare, (<=)
-- Защото могат да се имплементират взаимно
data Ordering' = LT' | EQ' | GT'

--class Eq a => Ord a where
--  compare :: a -> a -> Ordering
--  (<) :: a -> a -> Bool
--  (<=) :: a -> a -> Bool
--  (>) :: a -> a -> Bool
--  (>=) :: a -> a -> Bool
--  max :: a -> a -> a
--  min :: a -> a -> a

-- Можем да ги имплементираме с идеята че,
-- Even е като 0
-- Odd е като 1
-- Заради остатъците при делене на 2
instance Ord Parity where
  compare :: Parity -> Parity -> Ordering
  compare Odd Odd   = EQ
  compare Even Even = EQ
  compare Odd Even  = GT
  compare Even Odd  = LT   
  
  (<=) :: Parity -> Parity -> Bool
  Odd  <= Odd  = True
  Even <= Even = True
  Odd  <= Even = False
  Even <= Odd  = True


-- Закони за Ord (частична наредба):
-- За всяко x y z:
--   x <= x
--   x <= y && y <= x -> x == y
--   x <= y && y <= z -> x <= z

{-
-- Num
instance Num Parity where
  fromInteger :: Integer -> Parity
  fromInteger n
    | n `mod` 2 == 0 = Even
    | otherwise      = Odd
    
  
  (+) = undefined

  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined
  -- (-) = undefined
-}



-- Ако нещата са достатъчно прости,
-- може да накараме Haskell да генерира инстанциите
data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Enum, Show, Read)

data Shape
  = Circle Double
  | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = 3.14 * r^2
area (Rectangle a b) = a * b


data List a = Nil | Cons a (List a)

listLength :: List a -> Int
listLength Nil        = 0
listLength (Cons a l) = 1 + listLength l 

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _  nv Nil         = nv
listFoldr op nv (Cons x xs) = op x $ listFoldr op nv xs


-- тотални функции
data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr

--eval :: Expr -> Int
--eval = undefined

data BTree a
  = Empty
  | BNode (BTree a) (BTree a)


data Nat
  = Zero
  | Succ Nat
  deriving Show

--- Имплементирайте нужните функции за да бъде
--  Nat инстанция на класовете Eq, Ord, Num
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Zero     == Zero     = True
  (Succ a) == (Succ b) = a == b
  _        == _        = False

  --(/=) :: Nat -> Nat -> Bool
  --(/=) = undefined


-- Изберете или compare или (<=) да реализирате
instance Ord Nat where
--  compare :: Nat -> Nat -> Ordering
--  compare = undefined

  (<=) :: Nat -> Nat -> Bool
  Zero <= Zero     = True
  Zero <= (Succ _) = True
  _    <= _        = False

instance Num Nat where
  fromInteger :: Integer -> Nat
  fromInteger 0 = Zero
  fromInteger n = (Succ $ fromInteger (n - 1)) 

  (+) :: Nat -> Nat -> Nat
  Zero     + Zero     = Zero
  (Succ a) + Zero     = (Succ a)
  Zero     + (Succ a) = (Succ a)
  (Succ a) + (Succ b) = (Succ $ Succ (a + b))

  (*) :: Nat -> Nat -> Nat
  _    * Zero = Zero
  Zero * _    = Zero
  (Succ a) * (Succ b) = foldr (+) Zero (replicate (times (Succ a)) (Succ b))
    where times Zero     = 0
          times (Succ n) = 1 + times n


  abs = undefined
  signum = undefined

  -- нямаме отрицателни естествени числа, тъй че следните функции губят смисъла си
  negate = undefined
  (-) = undefined

-- И за последно едно предизвикателство,
-- напишете функция, която намира предишното естествено число,
-- тоест изважна единица от него. Ако е нула си остава нула.
--
-- pred $ Succ $ Succ $ Zero --> Succ $ Zero
-- pred Zero --> Zero
pred' :: Nat -> Nat
pred' Zero     = Zero
pred' (Succ a) = a