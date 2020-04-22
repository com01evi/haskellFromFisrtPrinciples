module Chapter22.Reader(
  composed,
  fmapped,
  tupled,
  doubler,
  getDogRM2,
  x1,
  x2,
  x3,
  toymain
)where

import Data.Char
import Data.Maybe
import Data.Monoid
import Control.Applicative

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

x = [1..3]
y = [4..6]
z = [7..9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y 

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> Maybe (Integer, Integer)
x3 n = (,) <$> z' n <*> z' n

summed :: (Num a) => (a,a) -> a
summed = uncurry (+)

bolt :: Integer -> Bool
bolt n = n>3 && n<8

toymain :: IO ()
toymain = do
  print $ sequenceA [Just 3 , Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> x1
  print $ fmap summed x2
  print $ bolt 7
  print $ fmap bolt z
  print $ foldMap (All) $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ fmap sequA s'
  print $ bolt $ fromMaybe 0 s'
  print $ fmap bolt s'
sequA :: Integral a => a -> [Bool]
sequA n = sequenceA [(>3),(<8), even] n

s' :: Maybe Integer
s' = summed <$> x1


