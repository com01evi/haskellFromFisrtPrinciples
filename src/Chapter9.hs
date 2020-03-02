module Chapter9
    ( 
    maybeTail,
    maybeHead,
    myenumFromTo,
    myWords,
    mydropWhile,
    mytakeWhile,
    firstSen,
    myLines,
    sentences,
    acro,
    myTuple,
    atoz,
    mySplitAt,
    myTake,
    myDrop,
    itIsMystery,
    boolMap,
    myEven,
    myOdd,
    howManyMul3,
    deleteArticle,
    myZip,
    myZipWith,
    myZip2,
    toUpperTheFirstChar,
    caesarCipher,
    unCaesar,
    myAny,
    myAny2,
    myOr,
    myElem1,
    myElem2,
    myReverse1,
    myReverse2,
    myMaximumBy,
    myMinimumBy,
    concatWithSpace,
    ) where

import Data.Bool (bool)
import Data.Char
import Control.Monad

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_:[]) = Nothing
maybeTail (_:xs) = Just xs

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

myenumFromTo :: (Enum a, Eq a) => a -> a-> [a]
myenumFromTo i n
  | i == n = [n]
  | otherwise = i : myenumFromTo (succ i) n

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x: myTake (n-1) xs

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (x:xs) = myDrop (n-1) xs

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n = (,) <$> (myTake n) <*> (myDrop n)

myWords :: String -> [String]
myWords = splitSentenseWith ' '

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile  _ [] = [] 
mydropWhile p (x:xs)
  | p x = mydropWhile p xs
  | otherwise = (x:xs)

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ [] = []
mytakeWhile p (x:xs) 
  | p x = x: mytakeWhile p xs
  | otherwise = []

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = splitSentenseWith '\n'

splitSentenseWith :: Char -> String -> [String]
splitSentenseWith _ [] = []
splitSentenseWith c str = takeWhile (c/=) str : splitSentenseWith c (drop 1 (dropWhile (c/=) str))

acro :: String -> String
acro xs = [x |x <- xs, elem x ['A'..'Z']]

mySqr :: [Int]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Int]
myCube = [x^3 | x <- [1..5]]

myTuple :: [(Int, Int)]
myTuple = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

atoz :: String
atoz = ['a'..'z']

itIsMystery xs = map (\x -> elem x "aeiou") xs

boolMap :: [Int] -> [Int]
boolMap xs = map (\x -> bool x (-x) (x==3)) xs

myEven :: Int -> Bool
myEven = (\x -> x `mod` 2 == 0)

myOdd :: Int -> Bool
myOdd = (\x -> x `mod` 2 /= 0)

howManyMul3 :: [Int] -> Int
howManyMul3 = length . (filter (\x -> x `mod` 3 == 0))

deleteArticle :: String -> String
deleteArticle =  concatWithSpace . (filter (\s -> s /= "the" && s /= "a" && s /= "an")) . words

concatWithSpace :: [String] -> String
concatWithSpace [] = []
concatWithSpace (x:[]) = x
concatWithSpace (x:xs) = x ++ " " ++ concatWithSpace xs

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y): zip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = (f x y): myZipWith f xs ys

myZip2 :: [a] -> [b] -> [(a,b)]
myZip2 = myZipWith (,)

--Chapter Exercise 2.
onlyUpper :: String -> String
onlyUpper = filter isUpper

--Chapter Exercise 3.
toUpperTheFirstChar :: String -> String
toUpperTheFirstChar [] = []
toUpperTheFirstChar (x:xs) = toUpper x : xs

--Chapter Exercise 4.
capitalizeAll :: String -> String
capitalizeAll = fmap toUpper

--Chapter Exercise 5 & 6.
capitalizedHead :: String -> Char
capitalizedHead = head . toUpperTheFirstChar

--Caesar cipher
caesarCipher :: Int -> String -> String
caesarCipher n = fmap (chr . shift n . ord)
  where shift :: Int -> Int -> Int
        shift n x
          | x + (n `mod` 26) > 122 = 96 + (x + (n `mod` 26) - 122)
          | otherwise = x + (n `mod` 26)

unCaesar :: Int -> String -> String
unCaesar n = fmap (chr . shift n . ord)
  where shift :: Int -> Int -> Int
        shift n x
          | x - (n `mod` 26) < 97 = 123 + (x - (n `mod` 26) - 97)
          | otherwise = x - (n `mod` 26)

--Writing my own standard library
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x = True
  | otherwise = myAny f xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 _ [] = False
myAny2 f (x:xs) = f x || myAny f xs

myElem1 :: (Eq a) => a -> [a] -> Bool
myElem1 _ [] = False
myElem1 a (x:xs)
  | a == x = True
  | otherwise = myElem1 a xs

myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 a = any (\x -> x == a)

myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 (x:xs) = myReverse1 xs ++ [x]

myReverse2 :: [a] -> [a]
myReverse2 = foldl (flip (:)) []

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = (=<<)

squishAgain :: [[a]] -> [a]
squishAgain = (=<<) id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldl1 (\acc x -> if f acc x == LT then x else acc)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldl1 (\acc x -> if f acc x == GT then x else acc)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
