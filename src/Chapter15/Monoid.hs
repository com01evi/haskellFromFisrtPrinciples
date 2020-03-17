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
    , IdentityIdentity
    , TwoIdentity
    , BoolConjIdentity
    , BoolDisjIdentity
    , combineLeftIdentity
    , combineRightIdentity
    , compLeftIdentity
    , compRightIdentity
    , memLeftIdentity
    , memRightIdentity
    ,memMain
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


instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a) (Identity b) = Identity $ mappend a b

type IdentityIdentity = Identity String -> Bool


instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

type TwoIdentity = Two String String -> Bool


instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

type BoolConjIdentity = BoolConj -> Bool


instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

type BoolDisjIdentity = BoolDisj -> Bool


instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\x -> mempty)
  mappend = (<>)

combineLeftIdentity :: Combine Int (S.Sum Int) -> Int -> Bool
combineLeftIdentity f x = unCombine (f <> mempty) x == unCombine f x

combineRightIdentity :: Combine Int (S.Sum Int) -> Int -> Bool
combineRightIdentity f x = unCombine (mempty <> f) x == unCombine f x


instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp (\x -> mempty)
  mappend = (<>)

compLeftIdentity :: Comp (S.Sum Int) -> (S.Sum Int) -> Bool
compLeftIdentity f x = unComp (f <> mempty) x == unComp f x

compRightIdentity :: Comp (S.Sum Int) -> (S.Sum Int) -> Bool
compRightIdentity f x = unComp (mempty <> f) x == unComp f x


instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend = (<>)

memLeftIdentity :: Mem Int String -> Int -> Bool
memLeftIdentity f x = runMem (f <> mempty) x == runMem f x

memRightIdentity :: Mem Int String -> Int -> Bool
memRightIdentity f x = runMem (mempty <> f) x == runMem f x

memMain :: IO ()
memMain = do
  let 
      f' = Mem $ \s -> ("hi", s + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
