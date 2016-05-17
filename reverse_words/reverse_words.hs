-- Rylan Dmello 15 May 2016

main = do
  line <- getLine
  if null line
    then do return () -- Return is like an inverse of the `<-` operator
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
