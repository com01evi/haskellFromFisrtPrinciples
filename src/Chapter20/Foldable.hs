{-# LANGUAGE KindSignatures #-}

module Chapter20.Foldable(
  treeFoldr
 ,MyMaybe(MyNothing,MyJust)
 ,myelem
 ,myminimum
 ,mymaximum
)where

import Chapter11(BTree(Leaf,Node))
import Chapter16.Functor(Identity(Identity))
import Data.Monoid

treeFoldr :: (a -> b -> b) -> b -> BTree a -> b
treeFoldr f acc Leaf = acc
treeFoldr f acc (Node left x right) = treeFoldr f (f x (treeFoldr f acc right)) left

fold :: (Monoid m) => BTree m -> m
fold Leaf = mempty
fold (Node left x right) = fold left <> x <> fold right

instance Functor BTree where
  fmap f Leaf = Leaf
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

instance Foldable BTree where
  foldMap f tree = fold $ fmap f tree

class MyFoldable (t :: * -> *) where
  myfold :: (Monoid m) => t m -> m
  toList :: t a -> [a]
  toList = myfoldr (:) []
  myfoldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable Identity where
  foldr f acc (Identity x) = f x acc
  foldl f acc (Identity x) = f acc x
  foldMap f (Identity x) = f x

data MyMaybe a = MyNothing | MyJust a

instance Foldable MyMaybe where
  foldr f acc MyNothing = acc
  foldr f acc (MyJust x) = f x acc

  foldl f acc MyNothing = acc
  foldl f acc (MyJust x) = f acc x

  foldMap f MyNothing = mempty
  foldMap f (MyJust x) = f x

mysum :: (Foldable t, Num a) => t a -> a
mysum = getSum . foldMap Sum

myproduct :: (Foldable t, Num a) => t a -> a
myproduct = getProduct . foldMap Product

myelem :: (Eq a, Foldable t) => a -> t a -> Bool
myelem e = getAny . foldMap (\x -> Any (x == e))

myminimum :: (Foldable t, Ord a) => t a -> Maybe a
myminimum = foldr (\x acc -> case acc of 
                               Just e -> if e < x then acc else Just x
                               Nothing -> Just x) Nothing

mymaximum :: (Foldable t, Ord a) => t a -> Maybe a
mymaximum = foldr (\x acc -> case acc of 
                               Just e -> if e > x then acc else Just x
                               Nothing -> Just x) Nothing
