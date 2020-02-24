module Chapter12
    (
      mkPerson
    , mkPerson2
    , replacethe
    , countTheBeforeVowel
    , countVowels
    , countVowels'
    , mkWord
    , natToInteger
    , integerToNat
    , Nat(Zero,Succ)
    , catMaybe
    , flipMaybe
    , myiterate
    , myUnfoldr
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

--String processing
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


--Validate the word
newtype Word' = Word' String deriving(Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if v < c 
           then Just $ Word' s
           else Nothing
  where 
        v :: Int
        v = countVowels s
        c :: Int
        c = countConsonants s


--It's only natural

data Nat = Zero | Succ Nat deriving(Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0 
natToInteger (Succ n) = (1+) (natToInteger n)

integerToNat :: Integer -> Maybe Nat
integerToNat n = if n < 0 then Nothing else Just $ integerToNat' n
  where integerToNat' :: Integer -> Nat
        integerToNat' 0 = Zero
        integerToNat' n = Succ $ integerToNat' (n-1)

--Small library for Maybe

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybe :: (Eq a) => [Maybe a] -> [a]
catMaybe = foldr (\x acc -> if x == Nothing then acc else (dropJust x):acc) []
  where dropJust :: Maybe a -> a
        dropJust (Just x) = x

flipMaybe :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe list = if elem Nothing list then Nothing else (Just . catMaybe) list

--Small library for Either

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> case x of 
  Left x -> x:acc
  Right x -> acc
  ) []

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> case x of 
  Left x -> acc
  Right x -> x:acc
  ) []

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' = (,) <$> lefts' <*> rights'

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right x) = Just $ either' id f (Right x)

myiterate :: (a -> a) -> a -> [a]
myiterate f x = x : f x : myiterate' f (f x)
  where myiterate' :: (a -> a) -> a -> [a]
        myiterate' f x = f x : myiterate' f (f x)

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f x = case f x of 
  Nothing -> []
  Just (y, z) -> y : myUnfoldr f z

myiterate' :: (a -> a) -> a -> [a]
myiterate' f = myUnfoldr (\x -> Just (x, f x))
