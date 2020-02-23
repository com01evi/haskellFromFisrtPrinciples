module Chapter12
    (
      mkPerson
    , mkPerson2
    , replacethe
    , countTheBeforeVowel
    , countVowels
    , countVowels'
    , mkWord
    ) where

import Chapter9(concatWithSpace)

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

data Unary a = Unary a deriving(Show)

r :: a -> f a
r = undefined

replacethe :: String -> String
replacethe = concatWithSpace . foldr (\x acc -> if x == "the" then "a":acc else x:acc) [] . words

countTheBeforeVowel :: String -> Int
countTheBeforeVowel = countTheBeforeVowel' . words

countTheBeforeVowel' :: [String] -> Int
countTheBeforeVowel' [] = 0
countTheBeforeVowel' (_:[]) = 0
countTheBeforeVowel' (x1:x2:xs) = if x1 == "the" && elem (head x2) "aeiou"
                                 then (1+) (countTheBeforeVowel' xs)
                                 else countTheBeforeVowel' xs

countVowels :: String -> Int
countVowels = length . filter (flip elem "aeiou")

countConsonants :: String -> Int
countConsonants = length . filter (not . flip elem "aeiou")

countVowels' :: String -> Int
countVowels' = foldr (\x acc -> if elem x "eaiou" then 1+acc else acc) 0

newtype Word' = Word' String deriving(Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if v < c 
           then Just $ Word' s
           else Nothing
  where 
        (v,c) = ((,) <$> countVowels <*> countConsonants) s
