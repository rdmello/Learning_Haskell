-- main = do line' <- fmap reverse getLine
--           putStrLn $ "You said " ++ line' ++ " backwards!"
--           putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"


-- CMaybe here is NOT a functor because it 
-- does not obey the identity rule of functors
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap _ CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- How is this working? (+) <$> [1..10] <*> [10,9..1]
-- See http://hackage.haskell.org/package/base-4.8.2.0/docs/src/GHC.Base.html#line-633
-- Apparently the Applicative <*> for [] is defined using list comprehensions!

myAction :: IO String
myAction = (++) <$> getLine <*> getLine



