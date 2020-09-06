-- from the book
quicksort :: Ord a => [a] -> [a]
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

-------------------------------------
generateExponents :: Integral a => a -> a -> [a]
generateExponents k l = generateNums k l 1 2
                        where generateNums k l x y = [x^k, y^l] ++ (generateNums k l (x + 1) (y + 1))
-----------------------------------------

-- 01
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

--03
addVectors :: Num t => (t, t) -> (t, t) -> (t, t)
addVectors (x1, x2) (y1, y2) = ( x1 + y1 , x2 + y2)   

--04
compress :: Eq t => [t] -> [t]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
  | x == y = compress (y:xs)
  | otherwise = x : compress (y:xs)

compress' :: Eq t => [t] -> [t]
compress' [] = []
compress' (x:xs) = x : compress' (dropWhile (==x) xs)

--05
duplicate :: [t] -> [t]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

--06
cycle' :: [t] -> [t]
cycle' [] = []
cycle' x = x ++ cycle' x

--07 - quick sort
quickSort :: Ord t => [t] -> [t]
quickSort [] = []
quickSort (pivot:rest) = quickSort smallerOrEqual ++ (pivot : quickSort larger)
  where smallerOrEqual = filter (<=pivot) rest
        larger = filter (>pivot) rest

--08 - merge sort
mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort firstHalf `merge` mergeSort secondHalf
  where mid = length xs `quot` 2
        (firstHalf, secondHalf) = splitAt mid xs
        merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | y < x = y : merge (x:xs) ys
          | otherwise = x : merge xs (y:ys)

--09
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([],[])
unzip' pairs = (map fst pairs, map snd pairs)

--10
reverse' :: [t] -> [t]
reverse' [] = []
reverse' x = (last x) : (reverse (init x))

reverse'' :: [t] -> [t]
reverse'' [] = []
reverse'' (x:xs) = (reverse'' xs) ++ [x]

--11
iterate' :: (t -> t) -> t -> [t]
iterate' op n = n : (iterate' op (op n))

--03
sumRange :: (Ord t, Num t) => t -> t -> t
sumRange s e 
    | s > e     = 0
    | otherwise = s + theRest
    where theRest = sumRange (s + 1) e
    
--08
repeated :: (Eq t, Num t) => (t -> t) -> t -> ( t -> t)
repeated f n
    | n == 0    = \x -> x
    | otherwise = \x ->  f ((repeated f ( n - 1)) x)

--08
countDigits :: Integer -> Integer 
countDigits n
    | n < 10    = 1
    | otherwise = 1 + (countDigits (n `div` 10))

--10
compose :: (t -> t) -> (t -> t) -> (t -> t)
compose f g = \x -> f (g x)

--13
reverseDigits :: Integer -> Integer
reverseDigits n
    | n < 10    = n
    | otherwise =  ten * lastNum +  (reverseDigits others)
        where others = n `div` 10
              ten = 10 ^ (countDigits others)
              lastNum = n `mod` 10

---------------------------------------------------------



