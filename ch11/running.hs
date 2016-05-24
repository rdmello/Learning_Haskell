import Control.Applicative

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

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

-- Newtypes etc

newtype ZipList' a = ZipList' {getZipList :: [a]}

newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)

instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x, y)

newtype CoolBool = CoolBool { getCoolBool :: Bool} deriving (Show)

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "Hello!"

-- Difference between type, data, and newtype

-- TYPE: used for making type synonyms eg
type IntList = [Int]
-- IntList can now basically be an alias for [Int]. 
-- Look at PhoneBook from global running.hs for another example.

-- NEWTYPE is for taking existing types and wrapping in new types
-- this makes it easier to make them instances of other classes
-- See CharList example above

-- DATA is for making your own data types. It is the most powerful 
-- of the three. 
