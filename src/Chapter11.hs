{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, NegativeLiterals #-}

module Chapter11
    (
    MyBool,
    Example(MakeExample),
    Person(Person,name,age),
    RecordProduct(first, second),
    myRecord,
    myRecord2,
    allProgrammers,
    allProgrammers2,
    List(Nil,Cons),
    makeTree,
    treeMap,
    makeListFromTree,
    treeFoldl,
    treeFoldr,
    vigenereCipher,
    capitalizeWord,
    capitalizeWords,
    capitalizeParagraph,
    splitSentence,
    reverseTaps,
    daphone,
    cellPhonesDead,
    convo,
    fingerTaps,
    mostPopularElement,
    countNumber,
    coolestLtr,
    coolestWord,
    Expr(Lit,Add),
    eval,
    printExpr,
    BTree(Leaf, Node)
    ) where
        
import Data.Int
import Data.Char
import Data.List
import Chapter9(caesarCipher,concatWithSpace)
import GHC.Exts(sortWith)

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

--newtype Cow = Cow Int deriving(Eq, Show)

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

--Exercises: Pity the Bool
--1.
data BigSmall = Big Bool | Small Bool deriving(Eq, Show)

--2.
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving(Eq, Show)

myNumba = Numba (-128)

data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving(Eq, Show)

type TwoQs = (QuantumBool, QuantumBool)

data Person = Person { name :: String
                     , age :: Int
                     }

--data Fiction = Fiction deriving (Show)
--
--data NonFiction = NonFiction deriving (Show)

--data BookType = FictionBook Fiction
 --             | NonFictionBook NonFiction
  --            deriving(Show)

type AuthorName = String

--data Author = Author (AuthorName, BookType)

data Author = Fiction AuthorName
            | NonFiction AuthorName
            deriving(Eq, Show)

type Gardener = String

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving(Show)

--
data GuessWhat = Chickenbutt deriving(Eq, Show)

data Id a = MkId a deriving(Eq, Show)

data Product a b = Product a b deriving(Eq, Show)

data Sum a b = First a | Second b deriving(Eq, Show)

data RecordProduct a b = RecordProduct{
                             first :: a,
                             second :: b
                         }deriving(Eq, Show)

newtype NumCow = NumCow Int deriving(Eq, Show)

newtype NumPig = NumPig Int deriving(Eq, Show)

data FarmHouse = FarmHouse NumCow NumPig deriving(Eq, Show)

type FarmHouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving(Eq, Show)

data BigFarmHouse = BigFarmHouse NumCow NumPig NumSheep deriving(Eq, Show)

type BigFarmHouse' = Product NumCow (Product NumPig NumSheep)

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving(Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving(Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving(Eq, Show)

data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo
            deriving(Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)           

bess' = (CowInfo "Bess" 4)

bess = First bess' :: Animal'

elmer' = Second (SheepInfo "elmer" 5 5)

elmer = Second elmer' :: Animal'

tom' = First (PigInfo "Tom" 5 True)

tom = Second tom' :: Animal'

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Int
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter = Twitter deriving(Eq, Show)

data AskFm = AskFm deriving(Eq, Show)

socialnetwork :: Sum Twitter AskFm
socialnetwork = First Twitter

myRecord :: RecordProduct Int Float
myRecord = RecordProduct 4 4.011

myRecord2 :: RecordProduct Int Float
myRecord2 = RecordProduct{
                first = 3,
                second = 4.001
            }

data OperatingSystem = Linux
                     | OpenBSD
                     | Mac
                     | Windows
                     deriving(Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | Purescript
              deriving(Eq, Show)

data Programmer = Programmer {
                      os :: OperatingSystem,
                      lang :: ProgLang
                  }deriving(Eq, Show)

feelingWizardly :: Programmer
feelingWizardly = Programmer{
                      os = OpenBSD,
                      lang = Haskell
                  }

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- [Linux, OpenBSD, Mac, Windows], y <- [Haskell, Agda, Idris, Purescript]]

allProgrammers2 :: [Programmer]
allProgrammers2 = do
    x <- [Linux, OpenBSD, Mac, Windows]
    y <- [Haskell, Agda, Idris, Purescript]
    return (Programmer x y)

newtype Name2 = Name2 String deriving(Eq, Show)

newtype Acres = Acres Int deriving(Eq, Show)

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving (Eq, Show)

data Farmer = Farmer {
                  name2 :: Name2,
                  acres :: Acres,
                  farmerType :: FarmerType
                  }deriving(Eq, Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

isDairyFarmer2 :: Farmer -> Bool
isDairyFarmer2 farmer = case farmerType farmer of
  DairyFarmer -> True
  _ -> False

data List a = Nil | Cons a (List a) deriving(Eq, Show)

data BTree a = Leaf | Node (BTree a) a (BTree a) deriving(Eq, Ord, Show)

myinsert :: (Eq a, Ord a) => a -> BTree a -> BTree a
myinsert x (Leaf) = Node Leaf x Leaf
myinsert x (Node left y right) 
  | x == y = Node left y right
  | x > y = Node left y (myinsert x right)
  | x < y = Node (myinsert x left) y right

treeMap :: (a -> b) -> BTree a -> BTree b
treeMap f Leaf = Leaf
treeMap f (Node left x right) = Node (treeMap f left) (f x) (treeMap f right)

makeTree :: (Ord a) => [a] -> BTree a
makeTree = foldr myinsert Leaf

makeListFromTree :: (Ord a) => BTree a -> [a]
makeListFromTree Leaf = []
makeListFromTree (Node left x right) = (makeListFromTree left) ++ [x] ++ (makeListFromTree right)

mysort :: (Ord a) => [a] -> [a]
mysort = makeListFromTree . makeTree 

treeFoldl :: (b -> a -> b) -> b -> BTree a -> b
treeFoldl f acc Leaf = acc
treeFoldl f acc (Node left x right) = treeFoldl f (f (treeFoldl f acc left) x) right

treeFoldr :: (a -> b -> b) -> b -> BTree a -> b
treeFoldr f acc Leaf = acc
treeFoldr f acc (Node left x right) = treeFoldr f (f x (treeFoldr f acc right)) left

listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr f acc [] = acc
listFoldr f acc (x:xs) = f x (listFoldr f acc xs)

--Chapter Exercises

vigenereCipher :: String -> String -> String
vigenereCipher [] [] = []
vigenereCipher (x:xs) (y:ys) = if x == ' ' then x:vigenereCipher xs ys
                                           else (chr . shift i . ord) x : vigenereCipher xs ys
  where shift :: Int -> Int -> Int
        shift n x
          | x + (n `mod` 26) > 122 = 96 + (x + (n `mod` 26) - 122)
          | otherwise = x + (n `mod` 26)
        i :: Int
        i = (ord y) - (ord 'a')
          

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf (x:xs) ys = elem x ys && isSubseqOf xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map ((,) <$> id <*> capitalize) . words
  where capitalize :: String -> String
        capitalize [] = ""
        capitalize (x:xs) = toUpper x:xs

capitalizeWord :: String -> String
capitalizeWord [] = ""
capitalizeWord (x:xs) = toUpper x:xs

capitalizeParagraph :: String -> String
capitalizeParagraph = concatWithDots . map capitalizeWord . splitSentence

splitSentence :: String -> [String]
splitSentence [] = []
splitSentence xs = takeWhile (/='.') xs : splitSentence (drop 2 (dropWhile (/='.') xs))

concatWithDots :: [String] -> String
concatWithDots [] = []
concatWithDots (x:[]) = x ++ "."
concatWithDots (x:xs) = x ++ ". " ++ concatWithDots xs

--Phone Exercise

type Key = Char
type CharList = String
data Daphone = Daphone [(Key, CharList)]

daphone = Daphone [('1',"1"),('2',"abc2"),('3',"def3"),('4',"ghi4"),('5',"jkl5"),('6',"mno6"),('7',"pqrs7"),('8',"tuv8"),('9',"wxyz"),('*',"^"),('0',"+ 0"),('#', ".,")]

convo :: [String]
convo = [ "Wanna play 20 questions"
        , "Ya"
        , "U 1st haha"
        , "Lol ok. Have you ever tasted alcohol"
        , "Lol ya"
        , "Wow ur cool haha. Ur turn"
        , "Ok. Do u think I am pretty Lol"
        , "Lol ya"
        , "Just makeing sure rofl ur turn"
        , "l is No.1 lllll"
        ]
type Presses = Int

reverseTaps :: Daphone -> Char -> [(Key,Presses)]
reverseTaps (Daphone dp) c = if isUpper c 
  then ('*', 1): reverseTaps' (Daphone dp) (toLower c)
  else reverseTaps' (Daphone dp) c

reverseTaps' :: Daphone -> Char -> [(Key,Presses)]
reverseTaps' (Daphone dp) c = [reverseTaps'' button]
  where reverseTaps'' :: Daphone -> (Key,Presses)
        reverseTaps'' (Daphone dp) = (((,) <$> fst <*> (myelemIndex c . snd)) . head) dp
        button :: Daphone
        button = Daphone (filter (elem c . snd) dp)
        myelemIndex :: (Eq a) => a -> [a] -> Int 
        myelemIndex a xs = case elemIndex a xs of
            (Just n) -> n + 1
            Nothing -> 0

cellPhonesDead :: Daphone -> String -> [(Key,Presses)]
cellPhonesDead dp = concat . map (reverseTaps dp)

fingerTaps :: [(Key, Presses)] -> Presses
fingerTaps = foldr (\x acc -> ((+acc) . snd) x) 0

mostPopularElement :: (Eq a) => [a] -> a
mostPopularElement = fst . head . reverse . sortWith snd . countNumber

countNumber :: (Eq a) => [a] -> [(a, Int)]
countNumber = foldr (\x acc -> if elem x (map fst acc) then (((,) <$> fst <*> (+1) . snd) . head . filter ((==x) . fst)) acc : filter ((/=x) . fst) acc
                                                               else ((x,1):acc)) []

coolestLtr :: [String] -> Char
coolestLtr = mostPopularElement . concat

coolestWord :: [String] -> String
coolestWord = mostPopularElement . words . concatWithSpace 

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add ex1 ex2) = eval ex1 + eval ex2

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add ex1 ex2) = (printExpr ex1) ++ " + " ++ (printExpr ex2)
