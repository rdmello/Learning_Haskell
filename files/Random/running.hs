import System.Random

main = do
  -- mkStdGen makes a standard generator
  -- for random
  print (threeCoins (mkStdGen 21))
  print (threeCoins (mkStdGen 22))
  print (threeCoins (mkStdGen 943))
  print (threeCoins (mkStdGen 944))
  print (take 10 $ randoms' (mkStdGen 21) :: [Bool])
  print (finiteRandoms 10 (mkStdGen 21) :: ([Bool], StdGen))
  return ()

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  -- Random takes a generator and returns a random number
  -- and another generator
  let (firstCoin, newGen) = random gen
      (secondCoin,newGen') = random newGen
      (thirdCoin ,newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
  let (value, newGen) = random gen
  in value:randoms' newGen

-- Like Randoms this generates a set of random numbers
-- But also provides the last StdGen in the output Tuple
finiteRandoms :: (Eq n, Num n, RandomGen g, Random a) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n-1) newGen
  in (value: restOfList, finalGen)
