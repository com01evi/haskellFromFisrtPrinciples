module Chapter15.Semigroup
    (
     NonEmpty(NonEmpty, (:|))
    ,Trivial
    ,TrivialAssoc
    ,semigroupAssoc
    ,Identity
    ,IdentityAssoc
    ,TwoAssoc
    ,BoolConjAssoc
    ,combineAssoc
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

newtype Combine a b = Combine {unCombine :: a -> b}

instance Show (Combine a b) where
  show _ = "Combine function"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CombineAssoc = Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Bool

combineAssoc :: Combine Int String -> Combine Int String -> Combine Int String -> Int -> Bool
combineAssoc (Combine f) (Combine g) (Combine h) x = unCombine (((Combine f) <> (Combine g)) <> (Combine h)) x == unCombine ((Combine f) <> ((Combine g) <> (Combine h))) x
