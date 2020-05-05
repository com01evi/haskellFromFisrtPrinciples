{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Chapter24.Marshalling(
  sectionJson,
  TestData,
  Host,
  Color,
  numlist,
  NumberOrString,
  parseSemVer2,
  parseSemVer,
  parseRelease,
  parseRelease',
  parseMetadata,
  parseMetadata'
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
                   
data NumberOrString = NOSS Int | NOSI String deriving(Eq, Show)

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

