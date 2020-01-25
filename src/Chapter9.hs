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
    boolMap
    ) where

import Data.Bool (bool)

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
  | p x = xs
  | otherwise = mydropWhile p xs

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ [] = []
mytakeWhile p (x:xs) 
  | p x = []
  | otherwise = x: mytakeWhile p xs

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = splitSentenseWith '\n'

splitSentenseWith :: Char -> String -> [String]
splitSentenseWith _ [] = []
splitSentenseWith c str = mytakeWhile (c==) str : myLines (mydropWhile (c==) str)

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
