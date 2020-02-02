module Chapter10
    (
    myfoldr,
    myfoldl,
    myReverse,
    e,
    ) where

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ a [] = a
myfoldr f a (x:xs) = f x (myfoldr f a xs)

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ a [] = a
myfoldl f acc (x:xs) = foldl f (f acc x) xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

--Exercise: Understanding Folds

--5. a)
a :: String
a = foldr (++) "" ["woot", "WOOT", "woot"]

b :: Char
b = foldr max 'a' "fear is the little death"

c :: Bool
c = foldr (&&) True [False, True]

d :: Bool
d = foldr (||) False [False, True]

e :: String
e = foldl (\acc x ->  acc ++ show x) "" [1..5]

f :: Int 
f = foldr const 0 [1..5]

g :: Char
g = foldr const 'a' "tacos"

h :: Char
h = foldl (flip const) 'a' "burritos"

i :: Int
i = foldl (flip const) 0 [1..5]
