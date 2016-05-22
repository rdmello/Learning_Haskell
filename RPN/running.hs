import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFcn [] . words
  where foldingFcn (x:y:ys) "*" = (x*y) : ys
        foldingFcn (x:y:ys) "+" = (x+y) : ys
        foldingFcn (x:y:ys) "-" = (y-x) : ys
        foldingFcn (x:y:ys) "/" = (y/x) : ys
        foldingFcn (x:y:ys) "^" = (y**x): ys
        foldingFcn (x:xs)   "ln"= log x : xs
        foldingFcn xs      "sum"= [sum xs]
        foldingFcn xs numberString = read numberString : xs
