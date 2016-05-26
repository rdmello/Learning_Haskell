
import Control.Monad 

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Int -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) -right) < 4 = Just (left + n, right)
  | otherwise                   = Nothing

landRight :: Int -> Pole -> Maybe Pole
landRight n (left, right) 
  | abs (right + n - left) < 4 = Just (left, right + n)
  | otherwise                  = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

foo :: Maybe String
foo = Just 3 >>= (\x -> 
      Just "!" >>= (\y -> 
      Just (show x ++ y)))


foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Nothing
  Just (show x ++ y)

-- Pattern matching works in do expressions
justH :: Maybe Char
justH = do
  (x:xs) <- Just "Hello"
  return x

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

-- guard :: (MonadPlus m) => Bool -> m ()
-- guard True = return ()
-- guard False = mzero

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c', r') <- [(c+2,r-1), (c+2,r+1), (c-2,r-1), (c-2,r+1)
              ,(c+1,r-2), (c+1,r+2), (c-1,r-2), (c-1,r+2)
              ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start



