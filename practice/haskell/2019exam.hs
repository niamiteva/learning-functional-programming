nums:: (Ord a) => (a ->  a) -> [a] -> [a]
nums f all =  [y | x <- (map (\x -> x : iterate f (f x)) all), y <- x, (f y) > y]

------------------------------------------------


type Capacity = Int
type DataBase = (String, Int)
type Server = (String, Capacity, [DataBase])

-- AAAAA

findBiggestDB :: (DataBase db, Int a, Ord a) => [db] -> a -> a
findBiggestDB [] max = max
findBiggestDB (db:dbl) max = if  size > max
                             then findBiggestDB dbl size
                             else findBiggestDB dbl max
                                where size = snd db 
                              
getServerName :: (Server s, String n) -> s -> n
getServerName (n, c , db) = n

getServerCap :: (Server s, Int c) -> s-> c
getServerCap (n, c, db) = c

getServerDB :: (Server s, DataBase db) => s -> db
getServerDB (n, c, db) = db

makeNamePercentList :: (Server s, String n, Int a) => [s] -> [(n, a)]
makeNamePercentList (s:sl) = ( serverName , percentCap) : makeNamePercentList sl
                              where serverName = getServerName s
                                    percentCap = maxCap - maxDBSize
                                    where maxCap = getServerCap s
                                          maxDBSize = findBiggestDB

findMinCapPercent :: (String n, Int c) => [(n, c)]  -> c -> n
findMinCapPercent [] _ server = server 
findMinCapPercent (s:sl) min server = if cap < min
                               then findMinCapPercent sl cap name
                               else findMinCapPercent sl min server
                                    where name = fst s
                                          cap = snd s

hasLargestDB :: (Server s, String a) => [s] -> a
hasLargestDB [] = "no DB"
hasLargestDB sl = findMinCapPercent nameCapList 1000000  ""
                  where nameCapList = makeNamePercentList sl

-- BBBB
sumAllDBSpace :: (DataBase db, Int a) => [db] -> a
sumAllDBSpace [] = 0
sumAllDBSpace (db:dbl) = size + (sumAllDBSpace dbl)
                         where size = snd db

maxFreeSpace :: (Server s, String a) => [s] -> a
maxFreeSpace [] max name = name
maxFreeSpace (s:sl) max name = if free > max 
                               then maxFreeSpace sl free n 
                               else maxFreeSpace sl max name
                               where n = getServerName s
                                     free = cap - (sumAllDBSpace db)
                                     where cap = getServerCap s
                                           db = getServerDB s

