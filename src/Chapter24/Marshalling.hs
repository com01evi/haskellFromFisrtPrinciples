{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Chapter24.Marshalling(
  sectionJson,
  TestData,
  Host,
  Color,
  numlist,
  NumberOrString(NOSS, NOSI),
  parseSemVer2,
  parseSemVer,
  parseRelease,
  parseRelease',
  parseMetadata,
  parseMetadata',
  parsePhone1,
  parsePhone2,
  parsePhone3,
  parsePhone4,
  parsePhone,
  parseComment,
  parseDate,
  parsePlan,
  parseSchedule
)where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.Monoid(Any(Any,getAny))
import Data.Scientific
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Text.RawString.QQ
import Text.Trifecta
import Text.Parser.Token
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Chapter24.Parser(int, strToInt)

sectionJson :: ByteString
sectionJson = [r|
  [
  { "section": {"host": "wikipedia.org"},
    "whatisit": {"red": "intoothandclaw"},
    "intdaze": 12,
    "arraydaze": [1,2,3]
  },
  { "section": {"host": "wikipedia.org"},
    "whatisit": {"red": "intoothandclaw"},
    "intdaze": 12,
    "arraydaze": [1,2,3]
  }
  ]
|]

numlist :: ByteString
numlist = "[1,\"hoge\",\"homu\"]"

data TestData = TestData{ section :: Host,
                          what :: Color,
                          intdayo :: Int,
                          list :: [Int]
                        }deriving (Eq, Show)

newtype Host = Host {host :: String}deriving(Eq,Show)

type Annotation = String

data Color = Red Annotation | Blue Annotation | Yellow Annotation deriving(Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) = TestData <$> v .: "section" 
                                  <*> v .: "whatisit"
                                  <*> v .: "intdaze"
                                  <*> v .: "arraydaze"
  parseJSON _ = fail "Expected an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "Expected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) = Red <$> v .: "red" <|>
                         Blue <$> v .: "blue" <|>
                         Yellow <$> v .: "yellow"
                   
data NumberOrString = NOSS Int | NOSI String deriving(Eq, Show, Read)

instance FromJSON NumberOrString where
  parseJSON (Number i) = return $ NOSS (base10Exponent i)
  parseJSON (String s) = return $ NOSI (unpack s)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving(Eq,Show)

parseSemVer :: Parser SemVer
parseSemVer = SemVer <$> integer <* dot <*> integer <* dot <*> integer <* many (char '-') <*> parseRelease <* many (char '+') <*> parseMetadata

parseSemVer2 :: Parser SemVer
parseSemVer2 = do
  major <- integer
  skipMany dot
  minor <- integer
  skipMany dot
  patch <- integer
  skipMany (char '-')
  release <- parseRelease
  skipMany (char '+')
  metadata <- parseMetadata
  return $ SemVer major minor patch release metadata

parseRelease' :: Parser NumberOrString
parseRelease' = do
  value <- try (NOSI <$> (some $ oneOf ['a'..'z'])) <|> try (NOSS <$> int) 
  skipMany (char '.')
  return value

parseRelease :: Parser Release
parseRelease = many parseRelease' 


parseMetadata' :: Parser NumberOrString
parseMetadata' = do
  value <- some $ oneOf (['a'..'z'] ++ ['0'..'9'])
  if getAny $ foldMap Any $ fmap (flip elem value) ['a'..'z'] 
  then do 
    skipMany dot
    return $ NOSI value
  else do
    skipMany dot
    return $ NOSS $ strToInt value

parseMetadata :: Parser Metadata
parseMetadata = many parseMetadata'

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving(Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = try parsePhone1 <|> try parsePhone2 <|> try parsePhone3 <|> try parsePhone4

parsePhone1 :: Parser PhoneNumber
parsePhone1 = do
  char '('
  npa <- int
  char ')'
  char ' '
  exc <- int
  char '-'
  line <- int
  return $ PhoneNumber npa exc line

parsePhone2 :: Parser PhoneNumber
parsePhone2 = do
  char '1'
  char '-'
  parsePhone3


parsePhone3 :: Parser PhoneNumber
parsePhone3 = do
  npa <- int
  char '-'
  exc <- int
  char '-'
  line <- int
  return $ PhoneNumber npa exc line

parsePhone4 :: Parser PhoneNumber
parsePhone4 = do
  list <- show <$> int
  let npa =  read $ take 3 list :: Int
  let nextList = drop 3 list
  let exc = read $ take 3 nextList :: Int
  let line = read $ drop 3 nextList :: Int
  return $ PhoneNumber npa exc line

data Schedule = Schedule Date [Plan] deriving(Eq, Show)

data Date = Date Year Month Day deriving(Eq, Show)

type Year = Int
type Month = Int
type Day = Int

data Plan = Plan Time Activity deriving(Eq, Show)

data Time = Time Hour Minute deriving(Eq, Show)

type Hour = Int
type Minute = Int

type Activity = String

parseComment :: Parser ()
parseComment = do
  char '-'
  char '-'
  skipMany $ char ' '
  skipMany $ oneOf (['a'..'z'] ++ [' '])
  skipMany newline
  return ()

myspace :: Parser ()
myspace = skipMany (char ' ') <|> skipMany (char '\n')

parseSchedule :: Parser Schedule
parseSchedule = do
  myspace
  skipMany parseComment
  myspace
  date <- parseDate
  plans <- some parsePlan
  return $ Schedule date plans

parseDate :: Parser Date
parseDate = do
  char '#'
  char ' '
  year <- int
  char '-'
  month <- int
  char '-'
  day <- int
  skipMany $ char ' '
  skipMany parseComment
  skipMany newline
  return $ Date year month day

parsePlan :: Parser Plan
parsePlan = do
  time <- parseTime
  char ' '
  activity <- some $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ [' ']
  skipMany $ char ' '
  skipMany parseComment
  skipMany newline
  return $ Plan time activity

parseTime :: Parser Time
parseTime = do
  hour <- int
  char ':'
  min <- int
  return $ Time hour min
