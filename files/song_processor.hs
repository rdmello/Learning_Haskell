import System.IO

main = do
  contents <- readFile "girlfriend.txt"
  putStr contents
  -- withFile' "girlfriend.txt" ReadMode (\handle -> do
  --   contents <- hGetContents handle
  --   putStr contents)
  -- handle <- openFile "girlfriend.txt" ReadMode
  -- contents <- hGetContents handle
  -- putStr contents
  -- hClose handle

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' filepath iomode fcn = do
  handle <- openFile filepath iomode
  result <- fcn handle
  hClose handle
  return result
