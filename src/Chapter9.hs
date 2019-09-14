module Chapter9
    ( 
    maybeTail,
    maybeHead,
    myenumFromTo,
    mywords,
    mydropWhile,
    mytakeWhile,
    firstSen,
    myLines,
    sentences,
    acro,
    myTuple,
    atoz
    ) where

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

mywords :: String -> [String]
mywords [] = []
mywords str = mytakeWhile (' '==) str : mywords (mydropWhile (' '==) str)

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
myLines [] = []
myLines str = mytakeWhile ('\n'==) str : myLines (mydropWhile ('\n'==) str )

acro :: String -> String
acro xs = [x |x <- xs, elem x ['A'..'Z']]

mySqr :: [Int]
mySqr = [x^2 | x <- [1..5], x^2 < 50]

myCube :: [Int]
myCube = [x^3 | x <- [1..5], x^3 < 50]

myTuple :: [(Int, Int)]
myTuple = [(x^2,y^3) | x <- [1..5], y <- [1..5], x^2 < 50, x^3 < 50]

atoz :: String
atoz = ['a'..'z']
