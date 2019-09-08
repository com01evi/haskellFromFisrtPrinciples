module Chapter9
    ( 
    maybeTail,
    maybeHead,
    myenumFromTo,
    mywords,
    mydropWhile,
    mytakeWhile
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
