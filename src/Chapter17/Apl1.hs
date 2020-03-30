{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter17.Apl1 (
)where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Semigroup a => Semigroup (ZipList a) where
 (ZipList xs) <> (ZipList ys) = liftA2 (<>) (ZipList xs) (ZipList ys)

instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList []
  mappend = liftA2 mappend

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq
