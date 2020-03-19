module Chapter16.Functor
    (
     f
    ,replaceWithp
    ,maybeList
    ) where

f = let either = Right "Hey" in head <$> either

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving(Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap f (Matter a) = Matter $ f a
  fmap _ WhatThisIsCalled = WhatThisIsCalled

data CountingBad a = Heisenberg Int a deriving(Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

replaceWithp :: a -> Char
replaceWithp = const 'p'

n = Nothing
w = Just "woohoo"
ave = Just "ave"

maybeList :: [Maybe String]
maybeList = [ave, n, w]
