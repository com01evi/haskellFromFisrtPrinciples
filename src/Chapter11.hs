{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Chapter11
    (
    MyBool,
    Example(MakeExample)
    ) where

data MyBool = MyTrue | MyFalse

data Doggies a = Husky a | Mastiff a deriving(Eq, Show)


--Exercises: Vehicles
data Price = Price Integer deriving(Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving(Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving(Eq, Show)

type Size = Integer

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving(Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m p) = m

data Example0 = Example0 deriving(Eq, Show)

data Example1 = Example1 Int deriving(Eq, Show)

data Example2 = Example2 Int String deriving(Eq, Show)

data Example = MakeExample Int deriving(Show)

newtype Goat = Goat Int deriving(Eq, Show, TooMany)

newtype Cow = Cow Int deriving(Eq, Show)

newtype TupleIntString = TupleIntString (Int,String) deriving(Eq, Show)

tooManyGoats :: Goat -> Bool
tooManyGoats (Goat x) = x > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 43

instance TooMany TupleIntString where
  tooMany (TupleIntString (n,_)) = tooMany n
--Exercises: Logic Goats

instance TooMany (Int,String) where
  tooMany (n,_) = tooMany n

instance TooMany (Int, Int) where
  tooMany (x,y) = tooMany (x+y)

instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (x,y) = tooMany (x+y)
