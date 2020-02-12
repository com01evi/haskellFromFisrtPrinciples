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
