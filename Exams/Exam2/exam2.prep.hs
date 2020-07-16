
type Show' = (String, (Int, Int), Int)

finishTime :: Show' -> Int
finishTime (_,(a, b), c) = a * 60 + b + c

first (x,_,_) = x

lastShow :: [Show'] -> String
lastShow [] = ""
lastShow shows = head [ a | (a,b,c) <- shows, finishTime (a,b,c) == maxFinishTime]
  where maxFinishTime = foldr (\show res -> max (finishTime show) res) 0 shows

getTimes :: Show' -> ((Int, Int), (Int, Int))
getTimes (_,(a,b),c) = ((a, b), (newa, newb))
  where newa = if (b + minutes) > 60 then (a + hours + 1) `mod` 24 
                                     else (a + hours) `rem` 24
        newb = if (b + minutes) > 60 then (b + minutes) `mod` 60 
                                     else b + minutes
        hours = c `div` 60
        minutes = c `mod` 60

convertStartToMins (a, b) = a * 60 + b 

findEarlier shows = (filter (\show ->
                    (convertStartToMins (fst (getTimes show))) == earlierTime) shows)         
  where earlierTime = foldr 
                        (\show res -> min (convertStartToMins (fst (getTimes show))) 
                                          res) (24 * 60 + 60) shows


--allPrograms [] = []
--allPrograms shows = findEarlier shows : allPrograms (removeEarlier shows)
  
--longestProgram shows = [ x | x <- shows] 




-- Group B
-- Task 1

findMinAndMax l = [minEl, maxEl]
  where minEl = foldr1 min l
        maxEl = foldr1 max l

member x l = if res == [] then False else True 
  where res = filter (\el -> el == x) l 


extremum ll = if res == [] then 0 else head res
  where minMaxL = map findMinAndMax ll 
        res = [x | x <- head minMaxL, (all (\el -> member x el) (tail minMaxL)) == True]






        






