module Chapter6
    ( TisAnInteger(..) 
    , EitherOr(..)
    ) where


data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn x) == (TisAn y) = x == y

data EitherOr a b = Hello a | GoodBye b

instance (Eq a,Eq b) => Eq (EitherOr a b) where
  Hello a == Hello b = a == b
  GoodBye a == GoodBye b = a == b
  Hello _ == GoodBye _ = False
  GoodBye _ == Hello _ = False

dividedThenadd :: (Fractional a) => a -> a -> a
dividedThenadd x y = x / y + 1

