--Var A

--Task1

type Name = String
type Size = Int
type Capacity = Int

type DataBase = (Name, Size)

type Server = (Name, Capacity, [DataBase])

getName (n,_,_) = n
getCapacity (_,c,_) = c
getDBLists (_,_,dbls) = dbls

-- a)

sumSizePerServer server =
   sum $ map snd $ getDBLists server


maxFree l = getName $ head $ filter (\s -> (getCapacity s - sumSizePerServer s) == maxFreeSpace) l 
  where maxFreeSpace = maximum $ map (\server -> getCapacity server - sumSizePerServer server ) l


-- maxFree [ ( "A" , 200 , [("a", 10), ("b",20) ] ) ,( "B" , 300 , [("c", 10), ("d",30) ] )  ]

-- b)

{-
sort [] = []
sort (x:xs) = sort greater ++ [x] ++ sort smaller
  where smaller = filter (<=x) xs
        greater = filter (>x) xs
-}

sortServers [] = []
sortServers sl = firstServer : sortServers rest
  where nameOfMaxFree = maxFree sl
        firstServer = head $ filter (\s -> getName s == nameOfMaxFree) sl
        rest = filter (\s -> getName s /= nameOfMaxFree) sl


sortDbList [] = []
sortDbList (pair:ps) = sortDbList greater ++ [pair] ++ sortDbList smaller
  where smaller = filter (\p -> snd p <= snd pair) ps
        greater = filter (\p -> snd p > snd pair) ps

sortDbPerServer (n,c,l) = (n,c, sortDbList l)

isPossibleFill [] _ result = result 
isPossibleFill _ [] _      = []
isPossibleFill (db:dbs) ((n,c,dblist):ss) result = 
    if canAdd 
        then isPossibleFill dbs ss (result ++ [toAdd]) 
        else []  
  where toAdd = (n,c, dblist ++ [db])
        canAdd = (getCapacity toAdd - sumSizePerServer toAdd) >= 0


isPossibleRemove name sl =
    if resultList == []
        then []
        else resultList
  where sortedServers = sortServers sl
        sorted = filter ((/=name) . getName) sortedServers
        dbListOfRedundantS = getDBLists $ sortDbPerServer $ head $ filter ((==name) . getName) sl
        resultList = isPossibleFill dbListOfRedundantS sorted []

-- isPossibleRemove "C" [ ( "A" , 200 , [("a", 10), ("b",20) ] ) ,( "B" , 300 , [("c", 10), ("d",30) ] ), ( "C" , 250 , [("j", 5), ("n",10)] )  ]

tryRemove name l =
    if result == []
        then l
        else result
  where result = isPossibleRemove name l

-- tryRemove "C" [ ( "A" , 200 , [("a", 10), ("b",20) ] ) ,( "B" , 300 , [("c", 10), ("d",30) ] ), ( "C" , 250 , [("j", 5), ("n",10)] )  ]

-- tryRemove "A" [ ( "A" , 200 , [("a", 10), ("b",20) ] ) ,( "B" , 300 , [("c", 10), ("d",30) ] ), ( "C" , 5 , [("j", 5), ("n",10)] )  ]

-- Task2
 
subSets [] = [[]]
subSets (x:xs) = (subSets xs) ++ (map (x:) $ subSets xs)

permutations [] = [[]]
permutations l = [a : b | a <- l, b <- permutations (delete a l)]
  where delete _ []     = []
        delete a (x:xs) = if a == x then xs else x : delete a xs


composeList [] = id
composeList (f:fs) = f . (composeList fs)

extendFl l = concat $ map (\x -> x:l) l

generateCompose 0 _ = [id] 
generateCompose n fl = 
    if n <= length fl 
        then map composeList nElSets
        else generateCompose n $ extendFl fl
  where nElSets = filter ((==n) . length) $ subSets fl

comps fl = [ result | n <- [0..] , result <-  generateCompose n fl ] 


