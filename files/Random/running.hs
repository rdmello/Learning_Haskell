import System.Random

main = do
  print (threeCoins (mkStdGen 21))
  print (threeCoins (mkStdGen 22))  
  return ()

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin,newGen') = random newGen
      (thirdCoin ,newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)
