module Chapter15.Monoid
    (
      list
    , list2
    , List(Nil,Cons)
    , append
    , myProduct
    , myHead
    , Listy(Listy)
    , monoidAssoc
    , monoidLeftIdentity
    , monoidRightIdentity
    , Optional(Nada,Only)
    , First'(First', getFirst')
    , FirstMappend
    , FstId
    , TrivialIdentity
    ) where

import Chapter11
import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import Test.QuickCheck
import Chapter15.Semigroup

list = mappend [1..3] [4..10]

list2 = mconcat [[1..3],[4..20]]

append :: List a -> List a -> List a
append Nil list@(Cons x _) = list
append list@(Cons x _) Nil = list
append (Cons x list) list2 = Cons x (append list list2)

sumResult = mappend (M.Sum 1) (M.Sum 2)

myProduct :: [Int] -> Int
myProduct = M.getProduct . mconcat . fmap M.Product

fiftyfive = foldl mappend mempty ([1,10] :: [M.Sum Int])

myHead :: [a] -> Maybe a
myHead = M.getFirst . mconcat . fmap (M.First . Just)

data Optional a = Nada | Only a deriving(Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only $ mappend a b

instance Semigroup a => Semigroup (Optional a) where
  (Only a) <> (Only b) = Only (a <> b)

newtype Listy a = Listy [a] deriving(Eq, Show)

instance Semigroup (Listy a) where
  (Listy l) <> (Listy l') = Listy $  l <> l'

instance Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy l) (Listy l') = Listy $ mappend l l'

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [ e
                                          , "! he said "
                                          , adv
                                          , " as he jumped into his car "
                                          , noun
                                          , "and drove off with his "
                                          , adj
                                          , " wife."
                                          ]


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> ( b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = (mempty <> m) == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m = (m <> mempty)  == m

newtype First' a = First' { getFirst' :: Optional a} deriving(Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return (First' Nada))
              ,(3, return (First' (Only a)) )
              ]

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only a)) _ = First' (Only a)
  mappend mempty (First' (Only a)) = (First' (Only a))
  mappend (First' Nada) (First' Nada) = First' Nada

instance Semigroup (First' a) where
  (First' (Only a)) <> _ = First' (Only a)
  (First' Nada) <> (First' (Only a)) = (First' (Only a))
  (First' Nada) <> (First' Nada) = First' Nada
type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

instance Monoid Trivial where
  mempty = Trivial
  mappend Trivial Trivial = Trivial

type TrivialIdentity = Trivial -> Bool
