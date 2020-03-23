{-# LANGUAGE FlexibleInstances, RankNTypes #-}

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
    ,functorIdentity
    ,functorCompose'
    ,functorCompose
    ,Identity(Identity)
    ,Pair(Pair)
    ,Two(Two)
    ,Three(Three)
    ,Three'(Three')
    ,Four(Four)
    ,Four'(Four')
    ,Possibly
    ,Sum
    ,Constant
    ,Wrap(Wrap)
    ,getInt
    ,functorMain
    ,WhoCares(ItDoesnt)
    ) where

import Test.QuickCheck
import Data.Char

f = let either = Right "Hey" in head <$> either

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving(Eq, Show, Read)

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
        changed = (read . ("123" ++) . show) <$> ioi
    in (*3) <$> changed


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == id f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap (g . f) x == (fmap g . fmap f) x

functorCompose' :: [Int] -> Bool
functorCompose' = functorCompose (1+) (2*)

instance Show (Int -> Int) where
  show _ = "function show instance"


--Exercises: Instances of Functor

--1.
newtype Identity a = Identity a deriving(Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

--2.
data Pair a = Pair a a deriving(Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

--3.
data Two a b = Two a b deriving(Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

--4.
data Three a b c = Three a b c deriving(Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

--5.
data Three' a b = Three' a b b deriving(Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

--6.
data Four a b c d = Four a b c d deriving(Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

--7.
data Four' a b = Four' a a a b deriving(Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d
    
liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (1+)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Possibly a = LolNope | Yeppers a deriving(Eq, Show)

instance Functor Possibly where
  fmap f LolNope = LolNope
  fmap f (Yeppers x) = Yeppers $ f x

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do 
    a <- arbitrary
    frequency [(1,return LolNope)
              ,(3,return $ Yeppers a)
              ]

data Sum a b = First a | Second b deriving(Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1,return $ First a)
              ,(3,return $ Second b)
              ]

data Constant a b = Constant a deriving(Eq,Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do
    x <- arbitrary
    return $ Constant x

data Wrap f a = Wrap (f a) deriving(Eq, Show)

instance (Functor f) => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap $ fmap f fa

getInt :: IO Int
getInt = read <$> getLine

functorMain :: IO ()
functorMain = do
  a <- getLine
  case all isNumber a of
    True -> print $ (read a) +1
    False -> error "Input was not number"

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]
