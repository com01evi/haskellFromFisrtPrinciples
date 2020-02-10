module Chapter11
    (
    MyBool
    ) where

data MyBool = MyTrue | MyFalse

data Doggies a = Husky a | Mastiff a deriving(Eq, Show)

data Price = Price Integer deriving(Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving(Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving(Eq, Show)

data Vehicles = Car Manufacturer Price | Plane Airline deriving(Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir
