module HsUnix
    ( FSItem(..)
    , FSCrumb(..)
    , FSZipper(..)
    , pwd
    , cd
    , cat
    , ls
    , rm
    , touch
    , mkdir
    , mv
    , getCurrentPath
    )
    where

import Data.List ()
import Data.Either ()

type Name = String  
type Data = String  
type Path = String

data FSItem = File Name Data | Folder Name [FSItem] 
                deriving (Eq, Ord, Show, Read)

type Before = [FSItem]
type After = [FSItem]
data FSCrumb = FSCrumb Name Before After
                deriving (Eq, Ord, Show, Read)

type FSZipper = (FSItem, [FSCrumb]) 

--data Either a b = Left a | Right b -- ==> success | error

-- ToDO persistent vector
-- todo 
-- [FSItem] [FSItem] -> [before the focused item] [after the focused item] 
-- MAIN OPERATIONS

--pwd [option] - print name of current/working directory
-- FSCrumbs - get all before the focused dir + the name of the focused dir
pwd :: FSZipper -> IO String
pwd fsz = return $ getCurrentPath fsz

-- cd dir - Change the shell working directory.
-- dir - relative dir/ and absolute ../
-- split dir by '/' and for each [subdirname] call dirUp or gotoDir
-- for each ../ call dirUp
-- for each dir/ call gotoDir
-- does not work always for relative paths
cd :: String -> FSZipper -> FSZipper
cd dir fsz = 
    let dirs = wordsBy '/' dir
    in case dirs of
        x -> if isAbsolute $ head x 
             then dirUp fsz
             else dirTo (head x) fsz 
        (x:xs) -> if isAbsolute x
                  then cd ls (dirUp fsz)
                  else cd ls (dirTo x fsz)
                  where ls = unwordsBy '/' xs

--cat file - print the data of file 
--make funct that takes the Data of File 
--if name is file 
--call that function and concat the data if multiple files 
-- if not file => error msg
-- does not change the FSCrumbs
-- > calls touch and display the Data
--cat :: [String] -> FSZipper -> String -> Either String FSZipper
--cat [] _ _= "cat expect name of file as argument" 
cat :: [String] -> FSZipper -> String -> Either String FSZipper
cat [file] fsz d = 
    let (File name dat, bs) = dirTo file fsz 
    in Left $ d ++ dat
cat (file:files) fsz d  = 
    if file == ">"
    then Right $ newFile (File file d) fsz
    else let (File name dat, bs) = dirTo file fsz
         in Left $ d ++ dd
         where Left dd = cat files fsz d

-- touch [options]... File...
touch :: [String] -> FSZipper -> FSZipper
--touch [] _ = putStrLn "touch expects file name as argument"
touch [name] fsz = newFile (File name "") fsz

-- mkdir [Options] folder... - Create new folder(s), if they do not already exist
mkdir :: [String] -> FSZipper -> FSZipper
--mkdir [] _ = putStrLn "mkdir expects file name as argument"
mkdir [name] (Folder folderName items, bs) = 
    let item = (Folder name [])
    in (Folder folderName (item:items), bs) 
                                                            
--ls [OPTION] [FILE] - list directory contents
--if no args => takes all none list items from the current dir from the zipper (our focused item)
--if args call funct that finds the dir without changing FSCrumbs and takes all none list items
ls :: [String] -> FSZipper -> IO String
ls [] (Folder name items, bc) = return $ unwords $ getFolderContent items
ls args fsz = 
    let newFSZ = cd (head args) fsz
    in ls [] newFSZ

-- rm [options]... file... - Remove files
rm :: [String] -> FSZipper -> FSZipper 
--rm [] _ = putStrLn "rm expect file name as argument"
rm [name] (Folder folderName items, bs) = 
    let (ls, f:rs) = break (nameIs name) items 
    in (Folder folderName (ls ++ rs), bs) 

-- mv
mv :: [String] -> FSZipper -> FSZipper  
--mv [] _ = putStrLn "mv expects file name and file as arguments"
mv [newName] (Folder name items, bs) = (Folder newName items, bs)  
mv [newName] (File name dat, bs) = (File newName dat, bs)  


-- GETTERS -----------------------------------------------------
getCurrentPath :: FSZipper -> String
getCurrentPath (focus, FSCrumb name ls rs:bc) = 
    (unwordsBy '/' $ getBeforeNames ls) ++ "/" ++ name ++ "/" ++ (getItemName focus) 

getFolderName :: FSItem -> String
getFolderName (Folder name dat) = name

getFileData :: FSItem -> String
getFileData (File n d) = d

getFileName :: FSItem -> String
getFileName (File name dat) = name

getItemName :: FSItem -> String
getItemName (Folder n _) = n
getItemName (File n _) = n

getBeforeNames::[FSItem] -> [String]
getBeforeNames [] = []
getBeforeNames (x:xs) = (getItemName x) : (getBeforeNames xs)

getFolderContent::[FSItem] -> [String]
getFolderContent [] = []
getFolderContent (x:xs)  = (getItemName x) : (getFolderContent xs)

--HELPERS-------------------------------------------------------

-- prelude's words rewritten to split string into list of words by given character
wordsBy :: Char -> String -> [String]
wordsBy c s =  case dropWhile (==c) s of
                      "" -> []
                      s' -> w : wordsBy c s''
                            where (w, s'') = break (== c) s'

-- prelude's unwords rewritten to concat list of strings into string with given separator
unwordsBy :: Char -> [String] -> String
unwordsBy _ [] =  ""
unwordsBy c ws =  foldr1 (\w s -> w ++ c:s) ws

isAbsolute :: String -> Bool
isAbsolute x = x == ".."

-- cd ../
dirUp :: FSZipper -> FSZipper  
dirUp (item, FSCrumb name ls rs:bs) = 
    let newItem = (ls ++ [item] ++ rs)
    in (Folder name newItem, bs)

-- cd dir/
dirTo :: Name -> FSZipper -> FSZipper  
dirTo name (Folder folderName items, bs) =   
    let (ls, item:rs) = break (nameIs name) items  
    in  (item, FSCrumb folderName ls rs:bs)  
  
-- check if item exists
nameIs :: Name -> FSItem -> Bool  
nameIs name (Folder folderName _) = name == folderName  
nameIs name (File fileName _) = name == fileName  

-- add file into current dir
newFile :: FSItem -> FSZipper -> FSZipper  
newFile item (Folder folderName items, bs) = 
    (Folder folderName (item:items), bs) 