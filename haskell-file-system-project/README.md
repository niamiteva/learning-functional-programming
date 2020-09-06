HsUnix :

- data type FSItem hold the to types of items in out file system => file and folder 
- data Type FSCrumb - bread crumbs => hold the name of the current dir, current full path, items before the current dir and items after it
- data type FSZipper - holds our breadcrumbs + the current folder

--pwd [option] - print name of current/working directory
- gets the path data from the breadcrubs form the zipper passed to the function 

-- TODO: fix it for relative path
-- cd [dir] - Change the shell working directory.
-- [dir] - relative dir/ and absolute ../
-- split [dir] by '/' and for each [subdirname] call dirUp or gotoDir
-- for each ../ call dirUp
-- for each dir/ call gotoDir

-- cd ../ or dirUP
- with the help of our breadcrumbs goes one directory up 

-- cd dir/ or gotoDir 
- in the cuurent dir in our zipper, checks for the folder in its items and adds it in the breadcrumbs and changes the current/focused item in the zipper

--cat
-- uses funct that takes the Data of File 
--if name is file 
--call that function and concat the data if multiple files 
-- if not file => error msg
-- does not change the FSCrumbs
-- > calls touch and display the Data

--ls [OPTION] [FILE] - list directory contents
--if no args => takes all none list items from the current dir from the zipper (our focused item)
--if args call funct that finds the dir without changing FSCrumbs and takes all none list items
--get all children => takes every non list item and appends it to the children list with fsitem names 

--rm 
--finds the file in the cuurent dir
--break the list with items where the file is and appends the other two parts from the list without the file we want to delete, then updates the fszipper

--touch and mkdir
--append fsitem to the items in our focused fsitem

--mv 
--rename file in the current dir 
--finds the file in the items list of our focused dir
--breaks the list where the file is and updates the fszipper with the new name where the file is

Main:

--initialize our fszipper 
--calls prompt to get cmd and passes to it the fszipper to be updated after executing every cmd
--prompt wait for input from the console if is not nothing calls execute
--execute checks the passed input and the cmd => if the cmd exists it executes it 
--at the end call prompt to get another cmd


Comments:
the projects have a lot error and things to be fixed 

REFS: 
http://learnyouahaskell.com/zippers
haskell docs
