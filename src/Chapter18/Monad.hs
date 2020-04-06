module Chapter18.Monad(
  ap
 ,joinIO
 ,printHogeHoge
 ,countMeMain
 ,nopeMain
 ,phEitherMain
 ,identityMain
 ,listMonadMain
 ,fliptype
)where

import Chapter17.Applicative
import Control.Monad(join,(>=>))
import Chapter16.Functor(Sum(First, Second), Identity(Identity))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

ap :: Monad f => f (a -> b) -> f a -> f b
ap af ax = af >>= (\f -> ax >>= (\x -> (return . f) x))



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
  arbitrary = CountMe <$> arbitrary <*> arbitrary
    

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

countMeMain :: IO ()
countMeMain = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g x = g x >>= f

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hi, how old are you?"

data Nope a = NopeDotJpg deriving(Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  NopeDotJpg >>= f = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

nopeMain :: IO ()
nopeMain = do
  let trigger :: Nope (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

data PhhhbbtttEither b a = Leftph a | Rightph b deriving(Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Leftph x) = Leftph $ f x
  fmap f (Rightph x) = Rightph x

instance Applicative (PhhhbbtttEither b) where
  pure = Leftph
  (Leftph f) <*> (Leftph x) = Leftph (f x)
  _ <*> (Rightph x) = (Rightph x)
  (Rightph x) <*> (Leftph y) = Rightph x

instance Monad (PhhhbbtttEither b) where
  (Leftph x) >>= f = f x
  (Rightph x) >>= _ = (Rightph x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(3, return $ Leftph y)
              ,(1, return $ Rightph x)
              ]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

phEitherMain :: IO ()
phEitherMain = do
  let trigger :: PhhhbbtttEither String (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

instance Monad Identity where
  Identity x >>= f = f x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityMain :: IO ()
identityMain = do
  let trigger :: Identity (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

instance Monad ListA where
  return = pure
  xs >>= f = concatList $ fmap f xs

listMonadMain :: IO ()
listMonadMain = do
  let trigger :: ListA (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y=  x >>= (\x1 -> y >>= (\y1 -> return $ f x1 y1))

a :: Monad m => m a -> m (a -> b) -> m b
a x f = x >>= (\x1 -> f >>= (\f1 -> return $ f1 x1))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = f x >>= (\x1 -> meh xs f >>= \xs1 -> return (x1:xs1))

meh2 :: Monad m => [a] -> (a -> m b) -> m [b]
meh2 [] _ = return []
meh2 (x:xs) f = (:) <$> f x <*> meh2 xs f

fliptype :: (Monad m) => [m a] -> m [a]
fliptype xs = meh2 xs id
