{-# LANGUAGE BlockArguments #-}
module Main where

import Data.Typeable (typeOf)
import HsUnix
    ( FSZipper,
      FSCrumb(FSCrumb),
      FSItem(Folder),
      pwd,
      cd,
      cat,
      touch,
      mkdir,
      ls,
      rm ,
      mv,
      getCurrentPath)

-- |Top level entry-point for shell.
main :: IO (Either String FSZipper)
main = do
        prompt ((Folder "/" []), [FSCrumb "" [] []])

-- |Loops, continually prompting, until exit status is sent.
prompt :: FSZipper -> IO (Either String FSZipper)
prompt fsz = do
    dir <-  putStr $ getCurrentPath fsz 
    input <- getLine
    case input of
      "" -> prompt fsz
      i -> execute fsz ((unwords . words) i)

execute :: FSZipper -> String -> IO (Either String FSZipper)
execute fsz i = do
    let cmd = head $ words i
        args = tail $ words i
        f = fsz
    case cmd of 
        "pwd" -> pwd f
        "cd" -> let fsz = cd (head args) f
                in return ""
        "ls" -> ls args f
        --"cat" -> let res = cat args f []
        "touch" -> let fsz = touch args f
                    in return ""
        "rm" -> let fsz = rm args f
                in return ""
        "mv" -> let fsz = mv args f
                in return ""
        "mkdir" -> let fsz = mkdir args f
                    in return ""
        -- ... and more

    prompt fsz 