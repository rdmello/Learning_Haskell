
-- import qualified Data.List as L
import Data.List
import qualified Data.Function as DF
import qualified Data.Char as DC
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geometry.Sphere as Sphere -- My very own module! 
import qualified Geometry.Cuboid as Cuboid -- My very own module! 
import qualified Geometry.Cube as Cube -- My very own module! 
import qualified Shapes -- My very own module! 

doubleU x y = doubleMe x + doubleMe y

doubleMe x = x + x

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

-- Non-typed definition. :t still gives [Char] -> [Char]
-- removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Typed definition
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Pattern Matching
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- Factorial Functions
factorial x = if x>1 then x * factorial (x-1) else 1

factorial' x = product [1..x]

factorial'' :: (Integral a) => a -> a
factorial'' 0 = 1
factorial'' n = n * factorial (n-1)

-- Using Pattern Matching with Tuples
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1+x2, y1+y2)

-- Extension of fst and snd for triples
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

head' :: [a] -> a
head' [] = error "Can't call head' on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The List is Empty"
tell (x:[]) = "The List has One Element: " ++ show x
tell (x:y:[]) = "The List has Two Elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- List length function using recursion and pattern matching
length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1+length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Patterns
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi    = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea
    
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

fibonacci :: (Integral a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci num = fibonacci (num-1) + fibonacci (num-2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of Empty List"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Maximum of Empty List Unknown"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = if x == y then True else elem' x ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [ a | a <- xs,  a <= x]
        biggerSorted = quicksort [ a | a <- xs,  a > x]
    in smallerSorted ++ [x] ++ biggerSorted

-- Higher Order Functions
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100
-- compareWithHundred x = compare 100 x

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: ( a -> b -> c ) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' lft ++ [x] ++ quicksort' rgt
    where lft = filter (<= x) xs
          rgt = filter (> x) xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) 
    | p x       = x : takeWhile' p xs
    | otherwise = []

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n*3 + 1  )

-- Foldl' renamed to foldl'' to avoid namespace
-- conflict with Data.List
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' x y [] = y
foldl'' x y (z:zs) = foldl'' (x) (x y z) zs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x = foldl (\a b -> a || (b == x)) False

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x y -> (f x) : y) []

-- Ch 7 Modules
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

encode :: Int -> String -> String
encode shift msg = 
    let ords = map DC.ord msg
        shifted = map (+ shift) ords
    in map DC.chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

-- Fixed version of findKey that allows for non-matching
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v): xs) = if key == k then Just v else findKey' key xs

-- Implement findKey as a foldl
findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key = foldr (\(k,v) acc -> if key==k then Just v else acc) Nothing

-- Implementing Data.Map's fromList function
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldl (\acc (k,v) -> Map.insert k v acc) Map.empty

phoneBook2 =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (\num1 num2 -> num1 ++ ", " ++ num2)

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

-- Ch 8: Types
-- See import Shapes directive above

-- Some important typeclasses: 
-- Eq (Equatable ==) -> Ord (Ordered GT, LT, EQ) -> Enum (Enumerable [..])
-- Show
-- Read
-- Bounded
-- Eq,Show -> Num 
-- -- Also, Integral -> Int, Integer (Use fromIntegral to convert)
-- Floating -> Float, Double

-- Point has the same name for the data type and for the value constructor
data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float
           | Rectangle Point Point deriving (Show)

-- Value constructors are functions that return a value of a data type. 
-- In the above statement, `Rectangle Float Float Float Float` is a 
-- value constructor. 

surface :: Shape -> Float -- Note the Type declaration..
-- Circle is not a type, but Shape is. 
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Value constructors are functions, so they can be partialed and mapped
-- map (Circle (Point 10 20)) [4..7] 

-- Record syntax
-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      , phoneNumber :: String
--                      , flavor :: String
--                      } deriving (Show)

-- Include Paamayim Nekudotayims as necessary! :D 

-- 8.3 Type Parameters
-- data Maybe a = Nothing | Just a
-- Here `a` is a type parameter. Will have 
-- `Maybe String` or `Maybe Int`, etc.
-- Doing `:t Just "Haha"` returns `Just "Haha" :: Maybe [Char]`
-- So type inference is used to figure out that the type parameter
-- should be array of Char in abv.
-- NB `:t Nothing` gives `Nothing :: Maybe a` which is polymorphic

-- Use type parameters only when necessary! (See pg 91 lower)

-- We could add typeclass constraints (like `(Int a) => ...`) 
-- to the data declaration, but it is not recommended since these 
-- constraints can be added to functions instead

-- Now Vector is parametrized just like Maybe above
data Vector a = Vector a a a deriving (Show)
-- Note that this Vector definition is broad enough to use strings
-- and chars even. 
-- Also note that `Vector a` is the TYPE constructor and
-- `Vector a a a` is the VALUE constructor

-- However, typeclass constraints are added to numeric methods. 
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = (i*l) + (j*m) + (k*n)

-- 8.4 Derived Instances
-- Remb. Typeclasses 101, with Eq -> Int

data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     } deriving (Eq, Show, Read) 
                     
-- `deriving (Eq)` tests if value constructors match and if fields are equal (deep?).

-- `data Bool = False | True deriving (Ord)` automatically assigns False LT True
-- Enum requires nullary (no fields/parameters) value constructors. It is for 
-- things that have successors and predecessors. 
-- Bounded is for things that have highest and smallest possible values

data Day = Monday | Tuesday | Wednesday | Thursday | Friday
         | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- 8.5 Type Synonyms `[Char] == String`
-- `type String = [Char]` ==> `type` KEYWORD. NOT used to define anything new, only
-- to show a relationship between previously defined types

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type IntMap v = Map.Map Int v

-- High-school locker example

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of 
        Nothing            -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- Example of failure modes (Right Left using Either) 
lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- 8.6 Recursive Data Structures

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
infixr 5 :-: -- Fixity declaration
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
-- List could be empty, or a concatenation of a head element and tail list. 

-- List addition
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ y      = y
(x :-: xs) .++ y = x :-: (xs .++ y)
-- Pattern matching worked here because it tries to match constructors

-- TREEES!!! TRESS!! xDDDD
-- Binary Search Tree (not balanced)
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

leftSubTree :: Tree a -> Tree a
leftSubTree EmptyTree = EmptyTree
leftSubTree (Node _ x _) = x

rightSubTree :: Tree a -> Tree a
rightSubTree EmptyTree = EmptyTree
rightSubTree (Node _ _ x) = x

-- Note how the (Ord a) type constraint is added to the function type declaration
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node y lt rt) 
    | x == y    = Node y lt rt
    | x < y     = Node y (treeInsert x lt) rt
    | x > y     = Node y lt (treeInsert x rt)

-- Find element in tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y lt rt)
    | x == y    = True
    | x < y     = treeElem x lt
    | x > y     = treeElem x rt

-- Build tree from list
listToTree :: (Ord a) => [a] -> Tree a
listToTree = foldr treeInsert EmptyTree

-- 8.7 Typeclasses 102


