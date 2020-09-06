import Data.List

-- task 1
contains :: (Ord a, Num a) => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) n = if x == n 
                  then True
                  else contains xs n

setIntersect:: (Ord a, Num a) => [a] -> [a] -> [a]
setIntersect _ [] = []
setIntersect [] _ = []
setIntersect xs ys = [x | x<-xs, (contains ys x) == True]

setDiff:: (Ord a, Num a) => [a] -> [a] -> [a]
setDiff xs [] = xs 
setDiff [] _ = []
setDiff xs ys = [x | x<-xs, (contains ys x) == False]

setSymDiff :: (Ord a, Num a) => [a] -> [a] -> [a]
setSymDiff xs [] = xs 
setSymDiff [] _ = []
setSymDiff xs ys = sort ([x | x<-xs, (contains ys x) == False] ++ 
                         [y | y<-ys, (contains xs y) == False])

setUnion :: (Ord a, Num a) => [a] -> [a] -> [a]
setUnion xs [] = xs
setUnion [] ys = ys
setUnion xs ys = sort (setSymDiff xs ys ++ setIntersect xs ys)

--task 2
type Item = (String, Integer)

maxByExpDate :: Item -> [Item] -> String
maxByExpDate x [] = fst x
maxByExpDate x (i:is) = if (fst i) > (fst x)
                        then  maxByExpDate i is
                        else maxByExpDate x is

minByExpDate :: Item -> [Item] -> String
minByExpDate x [] = fst x
minByExpDate x (i:is) = if (fst i) < (fst x)
                        then  minByExpDate i is
                        else minByExpDate x is

expiringItems :: [Item] -> (String, Integer, String)
expiringItems all@(i:is) = (minExpDate, expiredCount, oldest) 
  where minExpDate = maxByExpDate i (filter (\x -> (snd x) >= 0) all)
        expiredCount = toInteger (length [ x | x<-all, (snd x) < 0])
        oldest = minByExpDate i (filter (\x -> (snd x) < 0) all)