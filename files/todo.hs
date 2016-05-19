import System.IO

main = do
  contents <- getLine
  appendFile "todo.txt" (contents ++ "\n")
