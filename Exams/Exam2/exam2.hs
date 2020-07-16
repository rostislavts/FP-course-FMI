

-- Task 2

isProgression a b c = b + d == c
  where d = b - a

-- Base
generateAn 0 = 1
generateAn 1 = 1
generateAn n = an
  where an = head [ x | x <- [1..], not (any (\k -> isProgression (generateAn (n - 2*k)) 
                                                  (generateAn (n - k)) 
                                                  x) 
                             [1..(n `div` 2)]) ]

forestFire = map generateAn [0..]


-- Task 3

type Item = (String, String, Double) -- Name, Category, Price


first (a,_,_) = a
second (_,b,_) = b
third (_,_,c) = c

getCategories items = map second items

sumOfCategory category items = foldr (\item r -> (third item) + r ) 0 (filter (\item -> (second item) == category) items)

calcPrice category name items =  foldr (\item r -> (third item) + r) 0 (filter (\item -> (first item) ==  name && 
                                                   (second item) == category ) items)

findName category items = fst (head (filter (\pair -> (snd pair) == maxPrice) allPairs))
  where allPairs = [ (name, price) | name <- (map first items),   
                                            price <- [(calcPrice category name items)] ]
        maxPrice = foldr max 0 (map snd allPairs)                                    


convertToTriples items = [ (category, sumPrice, name) | category <- (getCategories items),
                                                        sumPrice <- [(sumOfCategory category items)],
                                                        name <- [(findName category items)] ] 

-- Example 
{-
[ ("A", "Glass", 24), ("B", "Jacket", 50), ("B", "Glass", 50), ("C", "Jacket", 60) ]
-}