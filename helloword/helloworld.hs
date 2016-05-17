-- Rylan Dmello 15 May 2016
-- Haskell Hello World

import Data.Char 
import Control.Monad

main = do
  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
    color <- getLine
    return color)
  putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
  mapM putStrLn colors
  -- putStr "Give me some imput: "
  -- l <- getLine
  -- putStrLn $ map toUpper l
  -- print "hello, world!"
  -- putStrLn "What is your name?"
  -- name <- getLine
  -- let bigName = map DC.toUpper name
  -- putStr' ("Hey ")
  -- putStr' (bigName) 
  -- putStrLn (", you rock!")
  -- c <- getChar
  -- when (c /= ' ') $ do
  --   putChar c
  --   main
  -- rs <- sequence [getLine, getLine, getLine]
  -- print rs


putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x 
  putStr' xs

-- Doing `:t putStrLn "asdf"` in GHCI gives "asdf" :: IO ()
-- This is an I/O action, with a result of empty tuple ().
