module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
 -- , groupOn
 -- , classifyOn
  , (&&&)
  , on
  ) where

import Data.Ord (comparing)

-- 00
group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (x:xs) = (x:ys) : group zs
                where (ys,zs) = span (== x) xs

-- 01
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp  = foldr (insertBy cmp ) []

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] =  [x]
insertBy cmp x al@(y:ys) =  if ( cmp x y ) == GT
                             then y : insertBy cmp x ys
                             else x : al

-- 02
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy _ [x] = [[x]]
groupBy cmp (x:xs) = (x:ys) : groupBy cmp zs
                     where (ys,zs) = span (cmp x) xs

-- 03.00
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op ret x y  = op (ret x) (ret y)

-- 03.01
(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) fp sp  x = ( fp x , sp x )

-- 04
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) 
                   . map (\x -> let y = f x in (y, x))

-- 05  
-- groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
-- groupOn f = let gr = groupBy ((==) `on` fst) . map (\x -> let y = f x in (y, x))
--             in [ [ snd x | x <- xs ] | xs <- gr]

-- -- 06
-- classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
-- classifyOn f x = groupOn f ( sortOn f x )
