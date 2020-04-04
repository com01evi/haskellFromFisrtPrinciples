module Chapter18.Monad(
  ap
 ,joinIO
 ,printHogeHoge
 ,countMeMain
)where

import Chapter17.Applicative
import Control.Monad(join)
import Chapter16.Functor(Sum(First, Second))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

ap :: Monad f => f (a -> b) -> f a -> f b
ap af ax = af >>= (\f -> ax >>= (\x -> (return . f) x))

instance Monad ListA where
  return = pure
  xs >>= f = concatList $ fmap f xs

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

joinIO :: IO ()
joinIO = join $ putStrLn <$> getLine

printHoge = putStrLn "hoge"

printHogeHoge = (,) <$> printHoge <*> printHoge

instance Monad (Sum a) where
  (First x) >>= _ = First x
  (Second x) >>= f = f x

data CountMe a = CountMe Integer a deriving(Eq, Show)

instance Functor CountMe where
  fmap f (CountMe n x) = CountMe n (f x)

instance Applicative CountMe where
  pure x = CountMe 0 x
  (CountMe n f) <*> (CountMe n' x) = CountMe (n+n') (f x)

instance Monad CountMe where
  (CountMe n x) >>= f = let (CountMe n' y) = f x
                        in CountMe (n+n') y

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = do
    n <- arbitrary
    x <- arbitrary
    return $ CountMe n x

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

countMeMain :: IO ()
countMeMain = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
