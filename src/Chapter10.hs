module Chapter10
    (
    myfoldr
    ) where

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ a [] = a
myfoldr f a (x:xs) = f x (myfoldr f a xs)

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ a [] = a
myfoldl f acc (x:xs) = foldl f (f acc x) xs
