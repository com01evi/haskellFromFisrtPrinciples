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
    ,Sum(First, Second)
    ,Wrap(Wrap)
    ,getInt
    ,functorMain
    ,WhoCares(ItDoesnt)
    ,Mu(InF, outF)
    ,D(D)
    ,More(L,R)
    ,TalkToMe
    ,Constant(Constant,getConstant)
    ,MyMaybe(MyNothing, MyJust)
    ,Pair2(Pair2)
    ,Big(Big)
    ,Bigger(Bigger)
    ,S(S)
    ) where

import Test.QuickCheck
import Data.Char
import GHC.Arr

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

--Chapter exercises

--4.
newtype Mu f = InF { outF :: f (Mu f)}

--5.
data D = D (Array Word Word) Int Int

--1.
data Sum2 b a = First2 a | Second2 b

instance Functor (Sum2 b) where
  fmap f (First2 a) = First2 $ f a
  fmap f (Second2 b) = Second2 b

--2.
data Company a c b = DeepBlue a c | Something b

instance Functor (Company a c) where
  fmap f (Something b) = Something $ f b
  fmap _ (DeepBlue a c) = DeepBlue a c

--3.
data More b a = L a b a | R b a b deriving(Eq, Show)

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--1.
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

--2.
data K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a

--3.
newtype Flip f a b = Flip (f b a) deriving(Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

--4.
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

--5.
data LiftItOut f a = LiftItOut (f a)

instance Functor g => Functor (LiftItOut g) where
  fmap f (LiftItOut ga) = LiftItOut (fmap f ga)

--6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)

--7.
data Ignoreone f g a b = IgnringSomething (f a) (g b)

instance (Functor g) => Functor (Ignoreone f g a) where
  fmap h (IgnringSomething fa gb) = IgnringSomething fa (fmap h gb)

--8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

--9.
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x list) = Cons (f x) (fmap f list)

--10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats goatA goatB goatC) = MoreGoats (fmap f goatA) (fmap f goatB) (fmap f goatC)

--11.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str (f x)
  fmap f (Read sg) = Read $ fmap f sg

newtype Constant a b = Constant { getConstant :: a} deriving(Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do
    x <- arbitrary
    return $ Constant x

data MyMaybe a = MyNothing | MyJust a deriving(Eq, Show)

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust $ f x

instance (Arbitrary a) => Arbitrary (MyMaybe a) where
  arbitrary = frequency [ (1, return MyNothing)
                        , (3, MyJust <$> arbitrary)
                        ]

data Pair2 a b = Pair2 a b deriving(Eq, Show)

instance Functor (Pair2 a) where
  fmap f (Pair2 x y) = Pair2 x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair2 a b) where
  arbitrary = Pair2 <$> arbitrary <*> arbitrary

data Big a b = Big a b b deriving(Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

data Bigger a b = Bigger a b b b deriving(Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data S n a = S (n a) a deriving(Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S fx x) = S (fmap f fx) (f x)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary
