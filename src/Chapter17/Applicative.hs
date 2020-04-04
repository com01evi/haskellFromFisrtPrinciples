module Chapter17.Applicative
    (
     Person
    ,mkPerson3
    ,MyValidate(Error,Safe)
    ,lookup1
    ,addMaybe1
    ,addMaybe2
    ,ListA(NilA,ConsA)
    ,concatList
    ,listMain
    ,flatMap
    ,mySortWith
    ,take'
    ,fold'
    ,ZipList'(ZipList')
    ,zipListMain2
    ,validateMain2
    ,pairMain
    ,twoMain
    ,threeMain
    ,three'Main
    ,fourMain
    ,four'Main
    ,combos
    ,stops
    ,vowels
    ) where

import qualified Data.Map as M
import Data.List (elemIndex)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Chapter16.Functor
import Control.Applicative(liftA3)

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyValidate a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Error a)
              ,(3, return $ Safe b)
              ]

instance (Eq a,Eq b) => EqProp (MyValidate a b) where
  (=-=) = eq

validateMain2 :: IO ()
validateMain2 = do
  let trigger :: MyValidate String (Int, Char, String)
      trigger = undefined
  quickBatch $ applicative trigger

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

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

newtype Const a b = Const {getConst :: a} deriving(Eq, Ord, Show)

instance Functor (Const a) where
  fmap f (Const x) = Const x

instance Monoid a => Applicative (Const a) where
  pure x = Const mempty
  Const m1 <*> Const m2 = Const $ m1 <> m2

newtype Name2 = Name2 String

newtype Address = Address String

data Person4 = Person4 Name2 Address

validateLength :: Int -> String -> Maybe String
validateLength n s = if length s > n
                     then Nothing
                     else Just s

mkName :: String -> Maybe Name2
mkName = fmap Name2 . validateLength 10

mkAddress :: String -> Maybe Address
mkAddress = fmap Address . validateLength 25

mkPerson4 :: String -> String -> Maybe Person4
mkPerson4 s1 s2 = Person4 <$> mkName s1 <*> mkAddress s2

--Exercises: Fixer Upper
--1.
fixUpper1 :: Maybe String
fixUpper1 = const <$> Just "Hello" <*> Just "World"

--2.
fixUpper2 :: Maybe (Int,Int,String,[Int])
fixUpper2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]


--List Applicative Exercise

data ListA a = NilA | ConsA a (ListA a) deriving(Eq, Show)

instance Functor ListA where
  fmap f NilA = NilA
  fmap f (ConsA x list) = ConsA (f x) (fmap f list)

instance Applicative ListA where
  pure x = ConsA x NilA
  NilA <*> _ = NilA
  _ <*> NilA = NilA
  (ConsA f fx) <*> xs = fmap f xs @@ (fx <*> xs)

(@@) :: (ListA a) -> (ListA a) -> (ListA a)
NilA @@ list = list
list @@ NilA = list
(ConsA x xs) @@ (ConsA y ys) = ConsA x (xs @@ (ConsA y ys))

replicateList :: (Applicative m) => Int -> m a -> m (ListA a)
replicateList cnt0 gen = loop cnt0
  where loop cnt
          | cnt <= 0 = pure NilA
          | otherwise = ConsA <$> gen <*> loop (cnt - 1)

instance Arbitrary1 ListA where
  liftArbitrary gen = sized $ \n ->
    do k <- choose (0,n)
       replicateList k gen

instance (Arbitrary a) => Arbitrary (ListA a) where
  arbitrary = liftArbitrary arbitrary

instance (Eq a) => EqProp (ListA a) where
  (=-=) = eq

listMain :: IO ()
listMain = do
  let trigger :: ListA (Int, Char, String)
      trigger = undefined
  quickBatch $ applicative trigger

take' :: Int -> ListA a -> ListA a
take' 0 _ = NilA
take' _ NilA = NilA
take' n (ConsA x list) = ConsA x (take' (n-1) list)

append :: ListA a -> ListA a -> ListA a
append NilA ys = ys
append (ConsA x xs) ys = ConsA x (append xs ys)

fold' :: (a -> b -> b) -> b -> ListA a -> b
fold' _ b NilA = b
fold' f b (ConsA x xs) = f x (fold' f b xs)

concatList :: ListA (ListA a) -> ListA a
concatList = fold' append NilA

flatMap :: (a -> ListA b) -> ListA a -> ListA b
flatMap f list = concatList $ fmap f list

mySortWith :: (Ord b) => (a -> b) -> [a] -> [a]
mySortWith f = fmap fst . qsortBySnd . fmap ((,) <$> id <*> f)
   
qsortBySnd :: Ord b => [(a,b)] -> [(a,b)]
qsortBySnd [] = []
qsortBySnd (x:xs) = (qsortBySnd smaller) ++ [x] ++ (qsortBySnd bigger)
  where
    smaller = filter (\n -> snd n <= snd x) xs
    bigger = filter (\n -> snd n > snd x) xs

newtype ZipList' a = ZipList' (ListA a) deriving(Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ foldr ConsA NilA $ repeat x
  ZipList' NilA <*> _ = ZipList' NilA
  _ <*> ZipList' NilA = ZipList' NilA 
  ZipList' fx <*> ZipList' xs = ZipList' $ go fx xs
    where go NilA _ = NilA
          go _ NilA = NilA
          go (ConsA f fx) (ConsA x xs) = ConsA (f x) (go fx xs)

instance Arbitrary1 ZipList' where
  liftArbitrary = fmap ZipList' . liftArbitrary

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = liftArbitrary arbitrary

zipListMain2 :: IO ()
zipListMain2 = do
  let trigger :: ZipList' (Int, Char, String)
      trigger = undefined
  quickBatch $ applicative trigger

--1.
instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq
  
pairMain :: IO ()
pairMain = do
  let trigger :: Pair (Int, Char, String)
      trigger = undefined
  quickBatch $ applicative trigger

--2.
instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two a f) <*> (Two b x) = Two (a<>b) (f x)

instance (Eq a, Eq b) => EqProp (Two a b) where
   (=-=) = eq 

twoMain :: IO ()
twoMain = do
  let trigger :: Two String (Int, Char, String)
      trigger = undefined
  quickBatch $ applicative trigger

--3.
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a p f) <*> (Three b q x) = Three (a<>b) (p<>q) (f x)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
   (=-=) = eq 

threeMain :: IO ()
threeMain = do
  let trigger :: Three String String (Int, Char, String)
      trigger = undefined
  quickBatch $ applicative trigger

--4.
instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f g) <*> (Three' b x y) = Three' (a<>b) (f x) (g y)

instance (Eq a, Eq b) => EqProp (Three' a b) where
   (=-=) = eq 

three'Main :: IO ()
three'Main = do
  let trigger :: Three' String (Int, Char, String)
      trigger = undefined
  quickBatch $ applicative trigger

--5.
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four a1 a2 a3 f) <*> (Four b1 b2 b3 x) = Four (a1<>b1) (a2<>b2) (a3<>b3) (f x)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
   (=-=) = eq

fourMain :: IO ()
fourMain = do
  let trigger :: Four String String String (Int, Char, String)
      trigger = undefined
  quickBatch $ applicative trigger

--5.
instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' a1 a2 a3 f) <*> (Four' b1 b2 b3 x) = Four' (a1<>b1) (a2<>b2) (a3<>b3) (f x)

instance (Eq a, Eq b) => EqProp (Four' a b) where
   (=-=) = eq

four'Main :: IO ()
four'Main = do
  let trigger :: Four' String (Int, Char, String)
      trigger = undefined
  quickBatch $ applicative trigger

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 (,,)

instance Applicative (Sum a) where
  pure = Second
  (First x) <*> _ = First x
  (Second f) <*> (First x) = First x
  (Second f) <*> (Second x) = Second $ f x
