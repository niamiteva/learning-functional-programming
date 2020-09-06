module HsUnix
    ( FSItem(..)
    , FSCrumb(..)
    , FSZipper(..)
    , Result
    , pwd
    , getCurrentPath 
    , cd
    , dirUp
    , gotoDir
    , isDir
    , isFile
    , getFolderName
    , cat
    , getFileData
    , getFileName
    , newFile
    , ls
    , getAllChildren
    , rm
    , touch
    , mkdir
    )
    where

import Data.List

type Name = String  
type Data = String  
type Path = String
type Res = IO String

data FSItem = File Name Data 
    | Folder Name [FSItem] 
    deriving (Show)
-- ToDO persistent vector
-- todo data Either = left a | right b ==> success | error
-- [FSItem] [FSItem] -> [before the focused item] [after the focused item] 
data FSCrumb = FSCrumb Name Path [FSItem] [FSItem] 
    deriving (Show)

type FSZipper = (FSItem, [FSCrumb]) 

data Result = FSZipper | Res

--pwd [option] - print name of current/working directory
-- FSCrumbs - get all before the focused dir + the name of the focused dir
pwd :: FSZipper -> IO String
pwd (focus, FSCrumb name path ls rs:bc) = return path

getCurrentPath :: FSZipper -> String
getCurrentPath (focus, FSCrumb name path ls rs:bc) = path -- ++ "/" ++ name

-- TODO: fix it for relative path
-- cd [dir] - Change the shell working directory.
-- [dir] - relative dir/ and absolute ../
-- split [dir] by '/' and for each [subdirname] call dirUp or gotoDir
-- for each ../ call dirUp
-- for each dir/ call gotoDir
cd :: [String] -> FSZipper -> FSZipper
cd [dir] fsz = do 
    dirs <- split '/' dir
    case dirs of
        x -> if isAbsolute x 
                    then dirUp fsz
                    else gotoDir x fsz 
        (x:xs) -> if isAbsolute x
                    then cd xs (dirUp fsz)
                    else cd xs (gotoDir x fsz)

isAbsolute :: String -> Bool
isAbsolute x = x == ".."

split :: Char -> String -> [String]
split c s = reverse (go s []) where
    go s' ws = case (dropWhile (\c' -> c' == c) s') of
        "" -> ws
        rem -> go ((dropWhile (\c' -> c' /= c) rem)) ((takeWhile (\c' -> c' /= c) rem) : ws)

-- cd ../
dirUp :: FSZipper -> FSZipper  
dirUp (item, FSCrumb name path ls rs:bs) = 
    let newPath = intercalate "/" ( init ( split  '/' path ))
        newItem = (ls ++ [item] ++ rs)
        folderName = getFolderName item
    in (Folder name newItem, [FSCrumb folderName (unwords ("/" ++ (words newPath))) (ls ++ [item] ++ rs) bs])

-- cd dir/
gotoDir :: Name -> FSZipper -> FSZipper  
gotoDir name (Folder folderName items, bs) =   
    let (ls, item:rs) = break (isDir name) items 
        newPath = (getCurrentPath (Folder folderName items, bs)) ++ "/" ++ folderName
    in  (item, FSCrumb folderName newPath ls rs:bs)  

-- check if dir exists    
isDir :: Name -> FSItem -> Bool  
isDir name (Folder folderName _) = name == folderName  

-- check if file exists
isFile :: Name -> FSItem -> Bool  
isFile name (File fileName _) = name == fileName

getFolderName :: FSItem -> String
getFolderName (Folder name dat) = name

--cat
--make funct that takes the Data of File 
--if name is file 
--call that function and concat the data if multiple files 
-- if not file => error msg
-- does not change the FSCrumbs
-- > calls touch and display the Data
cat :: [String] -> FSZipper -> [String] -> IO String 
--cat [] _ _= "cat expect name of file as argument" 
cat [x] (Folder name items, bs) da = 
    cat files (Folder name items, bs) dat = do
        case files of
            [x] -> let (ls, f:rs) = break (isFile x) items 
                    in return (unwords (dat ++ (words (getFileData f))))
            (x:xs) -> if x == ">"
                    then newFile (File (getFileName xs) (unwords dat))
                    else let (ls, f:rs) = break (isFile x) items 
                            in cat xs (Folder name items, bs) (dat ++ (words (getFileData f))) 

getFileData :: FSItem -> String
getFileData (File name dat) = dat

getFileName :: FSItem -> String
getFileName (File name dat) = name

newFile :: FSItem -> FSZipper -> FSZipper  
newFile item (Folder folderName items, bs) = 
    (Folder folderName (item:items), bs) 

--ls [OPTION] [FILE] - list directory contents
--if no args => takes all none list items from the current dir from the zipper (our focused item)
--if args call funct that finds the dir without changing FSCrumbs and takes all none list items
ls :: [String] -> FSZipper -> IO String
ls [] (Folder name items, bc) = return $ (unwords (getAllChildren (fst items) (tail items) []))
ls args fsz = let newFSZ = cd args fsz
              in ls [] newFSZ

getAllChildren :: FSItem -> [FSItem] -> [String] -> [String]
getAllChildren item items children = case (fst items) of
                                            x -> getAllChildren (fst items) (tail items) (children ++ (words (getItemName item)))
getAllChildren item items children = case (fst items) of
                                            x -> getAllChildren (fst items) (tail items) (children ++ (words (getItemName item)))

getItemName :: FSItem -> String
getItemName (Folder name dat) = name
getItemName (File name dat) = name

--rm
rm :: [String] -> FSZipper -> FSZipper 
--rm [] _ = putStrLn "rm expect file name as argument"
rm [name] (Folder folderName items, bs) = let (ls, f:rs) = break (isFile name) items 
                                                       in (Folder folderName (ls ++ rs), bs) 
rm [name] (Folder folderName items, bs) = let (ls, f:rs) = break (isDir name) items 
                                                       in (Folder folderName (ls ++ rs), bs) 

-- touch
touch :: [String] -> FSZipper -> FSZipper
--touch [] _ = putStrLn "touch expects file name as argument"
touch [name] (Folder folderName items, bs) = let item = (File name "")
                                            in (Folder folderName (item:items), bs) 

--mkdir
mkdir :: [String] -> FSZipper -> FSZipper
--mkdir [] _ = putStrLn "mkdir expects file name as argument"
mkdir [name] (Folder folderName items, bs) = let item = (Folder name [])
                                            in (Folder folderName (item:items), bs) 
--TODo 
-- mv
-- mv :: [String] -> FSZipper -> FSZipper  
-- mv [] _ = putStrLn "mv expects file name and file as arguments"
-- mv [newName] (Folder name items, bs) = (Folder newName items, bs)  
-- mv [newName] (File name dat, bs) = (File newName dat, bs)  
