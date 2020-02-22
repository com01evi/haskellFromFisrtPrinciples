module Chapter12
    (
      mkPerson
    , mkPerson2
    ) where

type Name = String

type Age = Int

data Person = Person Name Age deriving(Show)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | (name /= "") && (age >= 0) = Just (Person name age)
  | otherwise = Nothing

data PersonInvalid = EmptyName | MinusAge deriving(Show, Eq)

type ValidatePerson a = Either [PersonInvalid] a

ageCheck :: Age -> ValidatePerson Age
ageCheck age
  | (age >= 0) = Right age
  | otherwise = Left [MinusAge]

nameCheck :: Name -> ValidatePerson Name
nameCheck name 
  | name /= "" = Right name
  | otherwise = Left [EmptyName]

mkPerson2 :: Name -> Age -> ValidatePerson Person
mkPerson2 name age = mkPerson2' (nameCheck name) (ageCheck age)
  where mkPerson2' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
        mkPerson2' (Right name) (Right age) = Right $ Person name age
        mkPerson2' (Right name) (Left ys) = Left ys
        mkPerson2' (Left xs) (Right age) = Left xs
        mkPerson2' (Left xs) (Left ys) = Left (xs++ys)
