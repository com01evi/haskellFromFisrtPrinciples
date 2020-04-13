module Chapter21.Traversable(
  mygroup
 ,mygroupOn
 ,grouplen
 ,lowercaseEachInterest
 ,mytraverse
 ,mysequenceA
)where

import Data.Char(toLower)
import Chapter20.Foldable
import Chapter11(BTree(Leaf,Node))
import Morse

mygroup :: (Eq a) => [a] -> [[a]]
mygroup [] = []
mygroup (x:xs) = y : mygroup ys
  where y = filter (\n -> x == n) (x:xs)
        ys = filter (\n -> x /= n) xs

mygroupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
mygroupOn _ [] = []
mygroupOn f (x:xs) = y : mygroupOn f ys
  where y = filter (\n -> f x == f n) (x:xs)
        ys = filter (\n -> f x /= f n) xs

grouplen :: (a -> b) -> [[a]] -> [(b,Int)]
grouplen f = fmap ((,) <$> f . head <*> length)

lowercaseEachInterest :: [(Int, String)] -> [(Int, String)]
lowercaseEachInterest = fmap ((,) <$> fst <*> fmap toLower . snd)

class (Functor t, Foldable t) => MyTraverse t where
  mytraverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  mytraverse f = mysequenceA . fmap f
  mysequenceA :: (Applicative f) => t (f a) -> f (t a)
  mysequenceA = mytraverse id

instance MyTraverse [] where
  mytraverse _ [] = pure []
  mytraverse f (x:xs) = (:) <$> f x <*> mytraverse f xs

instance MyTraverse BTree where
  mytraverse _ Leaf = pure Leaf
  mytraverse f (Node left x right) = Node <$> (mytraverse f left) <*> (f x) <*> (mytraverse f right)

mymapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mymapM _ [] = return []
mymapM f (x:xs) = f x >>= (\y -> mymapM f xs >>= (\ys -> return (y:ys)))

sequence :: (Monad m) => [m a] -> m [a]
sequence = mymapM id
