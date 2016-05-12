
module Tutorial1 where

-- Haskell functions: no side effects
absolute :: Int -> Int
absolute x = if x < 0 then negate x else x

-- Data Types can be used recursively
data Geometry = Point Float Float
              | Circle Float Float Float
              | Rectangle Float Float Float Float
              | Composite [Geometry]
              deriving Show

-- Data Type 1: Enumeration (variants, but no components)
data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show

-- Data Type 2: Structures (one variant, multiple component)
data Person = Person String Int Int deriving Show -- name, age, shoe size

-- below fcn shows parametric polymorphism since it can accept any type
-- as input. "Parametric" because it results from parametrizing over a type
identity :: t -> t
identity x = x

-- Pattern matching
hd :: [a] -> a
hd []        = error "Cannot take head of empty list!"
hd (x:_)     = x

-- Q4
t1 :: [a] -> [a]
t1 [] = error "Cannot return tail of empty list"
t1 (_:xs) = xs

-- PM with tuples
first :: (a,b) -> a
first (x,y) = x

isZero :: Int -> Bool
isZero 0 = True
isZero n = False

area :: Geometry -> Float
area (Point x y) = 0
area (Circle x y r) = pi * r ^ 2
area (Rectangle t l r b) = (b-t) * (r-l)
area (Composite cs) = foldl (\acc x -> acc + (area x)) 0 cs

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- Stopped at pg 13








