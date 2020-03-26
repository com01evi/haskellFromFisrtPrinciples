module Chapter17.Applicative
    (
     Person
    ,mkPerson3
    ,MyValidate(Error,Safe)
    ,lookup1
    ,addMaybe1
    ,addMaybe2
    ) where

import qualified Data.Map as M
import Data.List (elemIndex)

data MyValidate a b = Error a | Safe b deriving(Eq, Show)

instance Functor (MyValidate a) where
  fmap _ (Error x) = Error x
  fmap f (Safe x) = Safe $ f x

instance Monoid a => Applicative (MyValidate a) where
  pure = Safe
  (Safe f) <*> (Error x) = Error x
  (Safe f) <*> (Safe x) = Safe $ f x
  (Error x) <*> (Safe y) = Error x
  (Error x) <*> (Error y) = Error $ x <> y

type Name = String
type Age = Int
data Person = Person Name Age deriving(Eq, Show)

data ErrorCode = NameEmpty | TooYoung deriving(Eq, Show)

nameChack :: String -> MyValidate [ErrorCode] Name
nameChack [] = Error [NameEmpty]
nameChack s = Safe s

ageCheck :: Int -> MyValidate [ErrorCode] Age
ageCheck n
  | n < 18 = Error [TooYoung]
  | otherwise = Safe n

mkPerson3 :: String -> Int -> MyValidate [ErrorCode] Person
mkPerson3 s n = Person <$> nameChack s <*> ageCheck n

lookup1 :: Int -> Maybe String
lookup1 x = M.lookup x $ M.fromList [ (3, "hello")
                                    , (4, "julie")
                                    , (5, "kbai")
                                    ]

lookup2 :: Int -> Maybe String
lookup2 x = M.lookup x $ M.fromList [ (6, "sup?")
                                    , (7, "chris")
                                    , (8, "aloha")
                                    ]

lookup3 :: Int -> Maybe Int
lookup3 x = M.lookup x $ M.fromList [ (6, 1)
                                    , (7, 2)
                                    , (8, 3)
                                    ]

lookup4 :: Int -> Maybe Int
lookup4 x = M.lookup x $ M.fromList [ (6, 10)
                                    , (7, 23)
                                    , (8, 9001)
                                    ]

addMaybe1 :: Maybe String
addMaybe1 = (++) <$> lookup1 3 <*> lookup2 6

addMaybe2 :: Maybe Int
addMaybe2 = (+) <$> lookup3 6 <*> lookup4 8

--Exercises: Lookups
--1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

--2.
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

--3.
x3 :: Maybe Int
x3 = elemIndex 3 [1..5]

y3 :: Maybe Int
y3 = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x3 <*> y3

--4.
x4 :: Maybe Integer
x4 = lookup 3 $ zip [1,2,3] [4,5,6]

y4 :: Maybe Integer
y4 = lookup 2 $ zip [1,2,3] [4,5,6]

summed :: Maybe Integer
summed = sum <$> ((,) <$> x4 <*> y4)
