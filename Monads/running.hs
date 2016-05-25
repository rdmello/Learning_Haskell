
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
