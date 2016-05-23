import Data.List

-- data Node = Node Road Road
--           | EndNode Road
-- 
-- data Road = Road Int Node
-- This Node-Road representation is unnecessary
-- since we usually split the road into sections. 

main = do 
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathPrice = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice

data Section = Section { getA :: Int
                       , getB :: Int
                       , getC :: Int
                       } deriving (Show)

-- Define a datatype for our roads
type RoadSystem = [Section]

-- Hardcode the road system in
heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

-- Label identifies which of path A, B, or C was chosen
data Label = A | B | C deriving (Show)

-- Path defines the roads taken so far and the distance for each.
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = 
  let priceA = sum $ map snd pathA 
      priceB = sum $ map snd pathB
      forwardPriceToA = priceA + a
      crossPriceToA = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      newPathToA = if forwardPriceToA <= crossPriceToA
                      then (A,a):pathA
                      else (C,c):(B,b):pathB
      newPathToB = if forwardPriceToB <= crossPriceToB
                      then (B,b):pathB
                      else (C,c):(A,a):pathA
  in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath

-- Parsing input to form a RoadSystem: 
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)




