module Chapter17.Applicative
    (
     Person
    ,mkPerson3
    ,MyValidate(Error,Safe)
    ) where

data MyValidate a b = Error a | Safe b deriving(Eq, Show)

instance Functor (MyValidate a) where
  fmap _ (Error x) = Error x
  fmap f (Safe x) = Safe $ f x

instance Monoid a => Applicative (MyValidate a) where
  pure = Safe
  (Safe f) <*> (Error x) = Error x
  (Safe f) <*> (Safe x) = Safe $ f x
  (Error x) <*> (Safe y) = Error x
  (Error x) <*> (Error y) = Error $ x <> y

type Name = String
type Age = Int
data Person = Person Name Age deriving(Eq, Show)

data ErrorCode = NameEmpty | TooYoung deriving(Eq, Show)

nameChack :: String -> MyValidate [ErrorCode] Name
nameChack [] = Error [NameEmpty]
nameChack s = Safe s

ageCheck :: Int -> MyValidate [ErrorCode] Age
ageCheck n
  | n < 18 = Error [TooYoung]
  | otherwise = Safe n

mkPerson3 :: String -> Int -> MyValidate [ErrorCode] Person
mkPerson3 s n = Person <$> nameChack s <*> ageCheck n
