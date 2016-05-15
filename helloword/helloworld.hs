-- Rylan Dmello 15 May 2016
-- Haskell Hello World

import qualified Data.Char as DC

main = do
  putStrLn "hello, world!"
  putStrLn "What is your name?"
  name <- getLine
  let bigName = map DC.toUpper name
  putStrLn ("Hey " ++ bigName ++ ", you rock!")

-- Doing `:t putStrLn "asdf"` in GHCI gives "asdf" :: IO ()
-- This is an I/O action, with a result of empty tuple ().
