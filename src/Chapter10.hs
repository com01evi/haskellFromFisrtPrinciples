module Chapter10
    (
    myfoldr,
    myfoldl,
    myReverse,
    e,
    threeAcc,
    theDatabase,
    getUTCTimeList,
    getMostRecentUTCTime,
    getDbNumberList,
    sumDb,
    avgDb
    ) where

import Data.Time

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

threeAcc :: [String] -> String
threeAcc = foldr (\x acc -> (take 3 x) ++ acc) ""

--Exercise: Database processing

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
              , DbNumber 9001
              , DbString "Hello, world!"
              , DbNumber 1000
              , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
              ]

--1.
isDbData :: DatabaseItem -> Bool
isDbData (DbDate a) = True
isDbData _ = False

getUTCTime :: DatabaseItem -> UTCTime
getUTCTime (DbDate u) = u

getUTCTimeList :: [DatabaseItem] -> [UTCTime]
getUTCTimeList = foldr (\x acc -> if isDbData x then getUTCTime x : acc else acc) []

--2.
isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber i) = True
isDbNumber _ = False

getDbNumber :: DatabaseItem -> Integer
getDbNumber (DbNumber i) = i

getDbNumberList :: [DatabaseItem] -> [Integer]
getDbNumberList = foldr (\x acc -> if isDbNumber x then getDbNumber x : acc else acc) []

--3.
getMostRecentUTCTime :: [DatabaseItem] -> UTCTime
getMostRecentUTCTime = foldr1 max . getUTCTimeList

compareUTCTime :: UTCTime -> UTCTime -> UTCTime
compareUTCTime t acc = if compare t acc == GT then t else acc

--4.
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (\x acc -> if isDbNumber x then (getDbNumber x) + acc else acc) 0

--5.
avgDb :: [DatabaseItem] -> Double
avgDb = uncurry (/) . getavgElem2
  where getavgElem :: [DatabaseItem] -> (Double,Double)
        getavgElem = ((,) <$> fromIntegral . sum <*> fromIntegral . length) . getDbNumberList
        getavgElem2 :: [DatabaseItem] -> (Double,Double)
        getavgElem2 = foldr (\x (y,z) -> if isDbNumber x then ((fromIntegral (getDbNumber x))+y,1+z) else (y,z)) (0.0,0.0)
