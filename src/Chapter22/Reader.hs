module Chapter22.Reader(
  composed,
  fmapped,
  tupled,
  doubler,
  getDogRM2
)where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev. cap

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

doubler :: Num a => (a -> a) -> (a ->a)
doubler f = (\x -> (f x) * 2)

newtype MyReader r a = MyReader {runReader :: (r -> a)} 

instance Functor (MyReader r) where
  fmap f (MyReader g) = MyReader (f . g)

instance Applicative (MyReader r) where
  pure x = MyReader (\_ -> x)
  (MyReader f) <*> (MyReader g) = MyReader (\x -> (f x) (g x))

instance Monad (MyReader r) where
  (MyReader f) >>= g = MyReader h
                       where h x = runReader (g (f x)) x

ask :: MyReader a a
ask = MyReader id

myliftA2 :: (Applicative m) => (a -> b -> c) -> m a -> m b -> m c
myliftA2 f m1 m2 = f <$> m1 <*> m2

asks :: (r -> a) -> MyReader r a
asks f = MyReader f

newtype HumanName = HumanName String deriving(Eq, Show)

newtype DogName = DogName String deriving(Eq, Show)

newtype Address = Address String deriving(Eq, Show)

data Person2 = Person2{
                 humanName :: HumanName
               , dogName :: DogName
               , address :: Address
               }deriving(Eq, Show)

data Dog = Dog {
             dogsName :: DogName
           , dogsAddress :: Address
           } deriving(Eq, Show)

getDogRM :: Person2 -> Dog
getDogRM = do
  name <- dogName
  a <- address
  return $ Dog name a

getDogRM2 :: MyReader Person2 Dog
getDogRM2 = do
  name <- asks dogName
  a <- asks address
  return $ Dog name a
