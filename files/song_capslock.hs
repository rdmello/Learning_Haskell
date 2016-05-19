import System.IO 
import Data.Char

main = do 
  contents <- readFile "girlfriend.txt"
  let caps_contents = map toUpper contents
  writeFile "girlfriendcaps.txt" caps_contents
