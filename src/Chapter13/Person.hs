module Chapter13.Person
    (
    gimmePerson
    ) where

type Name = String
type Age = Int

data Person = Person Name Age deriving(Show)

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving(Eq, Show)

validName :: Name -> Either [PersonInvalid] Name
validName [] = Left [NameEmpty]
validName name = Right name

validAge :: Age -> Either [PersonInvalid] Age
validAge age 
  | age < 0 = Left [AgeTooLow]
  | otherwise = Right age

mkPerson :: Name -> Age -> Either [PersonInvalid] Person
mkPerson name age = mkPerson' (validName name) (validAge age)
  where mkPerson' :: Either [PersonInvalid] Name -> Either [PersonInvalid] Age -> Either [PersonInvalid] Person
        mkPerson' (Left n) (Left a) = Left (n++a)
        mkPerson' (Left n) (Right age) = Left n
        mkPerson' (Right name) (Left a) = Left a
        mkPerson' (Right name) (Right age) = Right $ Person name age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Input name:"
  name <- getLine
  putStrLn $ "name: " ++ name
  putStr "Input age:"
  age <- getLine
  putStrLn $ "age: " ++ age
  printPerson $ mkPerson name (read age)

printPerson :: Either [PersonInvalid] Person -> IO ()
printPerson (Left list) = putStrLn $ "error occuerd, error was: " ++ show list
printPerson (Right person) = putStrLn $ "Yay! Successfully got a person" ++ show person
