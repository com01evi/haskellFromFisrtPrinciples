module Chapter20.Foldable(
treeFoldr
)where

import Chapter11(BTree(Leaf,Node))

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
