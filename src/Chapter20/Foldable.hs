{-# LANGUAGE KindSignatures #-}

module Chapter20.Foldable(
  treeFoldr
 ,MyMaybe(MyNothing,MyJust)
 ,myelem
 ,myminimum
 ,mymaximum
 ,mylength
 ,mynull
 ,filterF
 ,fizzBuzz
)where

import Chapter11(BTree(Leaf,Node))
import Chapter16.Functor(Identity(Identity), Constant(Constant,getConstant), MyMaybe(MyNothing, MyJust),Three(Three),Pair2(Pair2),Big(Big),Bigger(Bigger),S(S))
import Data.Monoid
import Chapter17.Applicative(ListA(NilA,ConsA))
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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
  foldMap = (fold .) . fmap

replicateTree :: Int -> Bool -> Bool -> Gen a -> Gen (BTree a)
replicateTree cnt0 lb rb gen = loop cnt0 lb rb
  where loop cnt lb rb
          | cnt <= 0 = pure Leaf
          | lb && rb = do l <- arbitrary
                          r <- arbitrary
                          Node <$> loop (cnt - 1) l r <*> gen <*> loop (cnt - 1) l r
          | lb = do l <- arbitrary
                    r <- arbitrary
                    Node <$> loop (cnt - 1) l r <*> gen <*> pure Leaf
          | rb = do l <- arbitrary
                    r <- arbitrary
                    Node <$> pure Leaf <*> gen <*> loop (cnt - 1) l r
          | otherwise = Node <$> pure Leaf <*> gen <*> pure Leaf

fizzBuzz :: [Int] -> [IO ()]
fizzBuzz (x:xs)
  | x `mod` 3 == 0 && x `mod` 5 == 0 = print (x, "fizzbuzz") : fizzBuzz xs
  | x `mod` 3 == 0 = print (x, "fizz") : fizzBuzz xs
  | x `mod` 5 == 0 = print (x, "buzz") : fizzBuzz xs
  | otherwise = fizzBuzz xs

instance Arbitrary1 BTree where
  liftArbitrary gen = do
       k <- choose (0,15)
       replicateTree k True True gen 

instance (Arbitrary a) => Arbitrary (BTree a) where
  arbitrary = liftArbitrary arbitrary

--instance (Arbitrary a) => Arbitrary (BTree a) where
--  arbitrary = frequency [ (1, return Leaf)
--                        , (3, Node <$> arbitrary <*> arbitrary <*> arbitrary)
--                        ]

class MyFoldable (t :: * -> *) where
  myfold :: (Monoid m) => t m -> m
  myfold = myfoldMap id
  toList :: t a -> [a]
  toList = myfoldr (:) []
  myfoldr :: (a -> b -> b) -> b -> t a -> b
  myfoldMap :: Monoid m => (a -> m) -> t a -> m
  myfoldMap f = myfoldr (\x acc -> f x <> acc) mempty

instance Foldable Identity where
  foldr f acc (Identity x) = f x acc
  foldl f acc (Identity x) = f acc x
  foldMap f (Identity x) = f x

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

mynull :: Foldable t => t a -> Bool
mynull t = foldMap (\x -> (Sum 1)) t == mempty

mylength :: (Foldable t) => t a -> Int
mylength = getSum . foldMap (const (Sum 1))

instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ y) = f y

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ y z) = f y <> f z

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ x y z) = f x <> f y <> f z

filterF :: (Foldable t, Applicative f, Monoid (f a)) => (a -> Bool) -> t a -> f a 
filterF p = foldMap (\x -> if p x then pure x else mempty)

instance Foldable (ListA) where
  foldMap _ NilA = mempty
  foldMap f (ConsA x list) = f x <> foldMap f list

instance Foldable (Pair2 a) where
  foldMap f (Pair2 x y) = f y

instance Foldable (Big a) where
  foldMap f (Big x z y) = f z <> f y

instance Foldable (Bigger a) where
  foldMap f (Bigger a b c d) = f b <> f c <> f d

instance (Foldable n) => Foldable (S n) where
  foldMap f (S fx x) = foldMap f fx <> f x
