module Chapter15.Semigroup
    (
     NonEmpty(NonEmpty, (:|))
    ,Trivial(Trivial)
    ,TrivialAssoc
    ,semigroupAssoc
    ,Identity(Identity)
    ,IdentityAssoc
    ,Two(Two)
    ,TwoAssoc
    ,BoolConj(BoolConj)
    ,BoolConjAssoc
    ,BoolDisj(BoolDisj)
    ,BoolDisjAssoc
    ,OrAssoc
    ,Combine(Combine,unCombine)
    ,combineAssoc
    ,Comp(Comp,unComp)
    ,compAssoc
    ,MyValidateAssoc
    ,validateMain
    ,memAssoc
    ,Mem(Mem,runMem)
    ) where

import Test.QuickCheck
import Data.Semigroup

data NonEmpty a = NonEmpty a | a :| NonEmpty a deriving (Eq, Ord, Show)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


newtype Identity a = Identity a deriving(Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool


data Two a b = Two a b deriving(Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary 
    return $ Two a b

type TwoAssoc = Two String String-> Two String String -> Two String String -> Bool


newtype BoolConj = BoolConj Bool deriving(Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj _) <> (BoolConj False) = BoolConj False
  (BoolConj False) <> (BoolConj _) = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


newtype BoolDisj = BoolDisj Bool deriving(Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


data Or a b = Fst a | Snd b deriving(Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  (Fst a) <> (Snd b) = Snd b
  (Snd a) <> (Fst b) = Snd a
  (Snd a) <> (Snd b) = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc = Or Int String -> Or Int String -> Or Int String -> Bool


newtype Combine a b = Combine {unCombine :: a -> b}

instance Show (Combine a b) where
  show _ = "Combine function"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

combineAssoc :: Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Int -> Bool
combineAssoc (Combine f) (Combine g) (Combine h) x = unCombine (((Combine f) <> (Combine g)) <> (Combine h)) x == unCombine ((Combine f) <> ((Combine g) <> (Combine h))) x


newtype Comp a = Comp { unComp :: (a -> a)}

instance Show (Comp a) where
  show _ = "Comp a"

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (\x -> f x <> g x)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

compAssoc :: Comp (Sum Int) -> Comp (Sum Int) -> Comp (Sum Int) -> Sum Int -> Bool
compAssoc f g h x = unComp ((f <> g) <> h) x == unComp (f <> (g <> h)) x


data MyValidate a b = MyFailure a | MySuccess b deriving(Eq, Show)

instance Semigroup a => Semigroup (MyValidate a b) where
  (MySuccess a) <> _ = MySuccess a
  (MyFailure a) <> (MySuccess b) = MySuccess b
  (MyFailure a) <> (MyFailure b) = MyFailure (a <> b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyValidate a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ MyFailure a)
              ,(3, return $ MySuccess b)
              ]

type MyValidateAssoc = MyValidate String Int -> MyValidate String Int -> MyValidate String Int -> Bool

validateMain :: IO ()
validateMain = do
  let failure :: String -> MyValidate String Int
      failure = MyFailure
      success :: Int -> MyValidate String Int
      success = MySuccess
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2


newtype Mem s a = Mem { runMem :: s -> (a,s)}

instance Show (Mem s a) where
  show _ = "mem functions"

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem (\s -> (((fst . f) s <> (fst . g) s), (snd . g) ((snd .f) s)))

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f

memAssoc :: Mem Int String -> Mem Int String -> Mem Int String -> Int -> Bool
memAssoc f g h x = runMem ((f <> g) <> h) x == runMem (f <> (g <> h)) x
