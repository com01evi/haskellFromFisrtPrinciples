module Chapter14.Arbitrary
    (
    sampleOriginalData,
    arbitrary,
    Identity,
    identityGenInt,
    Product,
    stringList,
    Lift,
    Sum
    ) where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

sampleOriginalData :: IO ()
sampleOriginalData = do
  sample trivialGen
  
data Identity a = Identity a deriving(Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen Int
identityGenInt = arbitrary

data Product a b = Product a b deriving(Eq, Show)

productGen :: (Arbitrary a, Arbitrary b) => Gen (Product a b)
productGen = do 
  a <- arbitrary
  b <- arbitrary
  return (Product a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Product a b) where
  arbitrary = productGen

stringList :: Gen Lift
stringList = elements [Lift "aaa",Lift "bbb",Lift "ccc"]

newtype Lift = Lift String deriving(Eq, Show)

instance Arbitrary Lift where
  arbitrary = stringList

data Sum a b = First a | Second b deriving(Eq, Show)

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a
        ,return $ Second b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequentSum

frequentSum :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
frequentSum = do
  a <- arbitrary
  b <- arbitrary
  frequency [ (1, return $ First a)
           , (3, return $ Second b)]

