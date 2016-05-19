import System.Environment
import Data.List
import System.Directory
import System.IO

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add)
           ,("view", view)
           ,("remove", remove)
           ]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args
  
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do 
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do 
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
  
--   let mode = args!!0
--       fileName = args!!1
--       payload = args!!2
--   if mode == "add" then todoAdd fileName payload else (if mode == "view" then todoView fileName else todoRemove fileName (read payload :: Int))
--   
-- todoAdd :: FilePath -> String -> IO ()
-- todoAdd = appendFile
-- 
-- todoView :: FilePath -> IO()
-- todoView file = do 
--   contents <- readFile file
--   print contents
-- 
-- todoRemove :: FilePath -> Int -> IO()
-- todoRemove file x = do
--   contents <- readFile file
--   let todos = words contents
--       newContents = delete (todos!!x) todos
--   writeFile file $ unwords newContents
--   



