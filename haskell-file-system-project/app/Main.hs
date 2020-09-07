module Main where

import HsUnix

-- |Top level entry-point for shell.
main :: IO ()
main = do 
        prompt fsz
        where fsz = FSZipper (Folder "/" [], FSCrumb "/" [] [])

-- |Loops, continually prompting, until exit status is sent.
prompt :: FSZipper -> FSZipper
prompt fsz = do
    dir <- getCurrentPath fsz 
    input <- getLine
    case input of
      Nothing -> prompt fsz
      Just i -> execute $ (unwords . words) i

execute :: String -> IO ()
execute fsz input = do
    cmd <- fst (words input)
    args <- tail (words input) 
    case cmd of 
        "pwd" -> pwd fsz
        "cd" -> cd args fsz
        "ls" -> ls args fsz
        "cat" -> cat args fsz []
        "touch" -> touch args fsz
        "rm" -> rm args fsz
        "mv" -> mv args fsz
        "mkdir" -> mkdir args fsz
        -- ... and more

    prompt fsz 