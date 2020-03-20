module Chapter16.Functor
    (
     f
    ,replaceWithP
    ,maybeList
    ,replaceWithPMain
    ,a
    ,b
    ,c
    ,d
    ,e
    ) where

f = let either = Right "Hey" in head <$> either

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving(Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap f (Matter a) = Matter $ f a
  fmap _ WhatThisIsCalled = WhatThisIsCalled

data CountingBad a = Heisenberg Int a deriving(Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

replaceWithP :: a -> Char
replaceWithP = const 'p'

n = Nothing
w = Just "woohoo"
ave = Just "ave"

maybeList :: [Maybe String]
maybeList = [ave, n, w]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twicelifted :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 Char)
twicelifted = (fmap . fmap) replaceWithP

twicelifted' :: [Maybe [Char]] -> [Maybe Char]
twicelifted' = twicelifted

thriceLifted :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

replaceWithPMain :: IO ()
replaceWithPMain = do
  putStr "replaceWithP' mayblmseList: "
  print $ replaceWithP' maybeList

  putStr "liftedReplace maybeList: "
  print $ liftedReplace maybeList

  putStr "liftedReplace' maybeList: "
  print $ liftedReplace' maybeList

  putStr "twicelifted maybeList: "
  print $ twicelifted maybeList

  putStr "twicelifted' maybeList: "
  print $ twicelifted' maybeList

  putStr "thriceLifted maybeList: "
  print $ thriceLifted maybeList

  putStr "thriceLifted' maybeList: "
  print $ thriceLifted' maybeList

--Exercises: Heavy Lifting

--1.
a = (1+) <$> read "[1]" :: [Int]

--2,
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

--3.
c = (*2) . (\x -> x - 2)

--4.
d = ((return '1' ++) . show) . (\x -> [x, 1..3])

--5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap (("123" ++) . show) ioi
    in (*3) <$> changed
