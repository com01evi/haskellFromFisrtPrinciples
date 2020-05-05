{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Chapter24.Parser(
  hoge,
  strToInt,
  testParse,
  parsePeople,
  testParse2,
  oneTwoThree,
  parseTestMain,
  int,
  p123,
  one,
  parserFraction,
  badFraction,
  alsoBad,
  shouldWork,
  shouldAlsoWork,
  testWithAttoParsec,
  testWithTrifecta,
  onlyInteger,
  parseNosmain,
  eitherOr,
  eitherOr',
  parseCorrectly,
  parseDecimalOrFraction
)where

import Text.Trifecta
import Text.Parser.Token
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Char
import Data.Ratio ((%))
import Data.String
import Data.Text.Internal(Text)
import Text.RawString.QQ

stop :: Parser a
stop = unexpected "stop"

one :: Parser ()
one = char '1' >> eof

one' :: Parser a
one' = one >> stop

oneTwo = one >> char '2'

oneTwo' = oneTwo >> stop

oneTwoThree = oneTwo >> char '3'

testParse :: Parser [Person] -> IO ()
testParse p = print $ parseString p mempty "Person 123, , taro\nPerson 24, , akie"

parsePerson :: Parser Person 
parsePerson = Person <$ string "Person" <* space <*> integer <* comma <* comma <*> many (oneOf ['a'..'z']) <* many newline

parsePeople :: Parser [Person]
parsePeople = many parsePerson

hoge = testParse parsePeople

data Person = Person { age :: Integer
                     , name :: String
                     }deriving(Eq, Show)

testParse2 :: Parser Char -> IO ()
testParse2 p = print $ parseString p mempty "123"

strToInt :: String -> Int
strToInt = snd . foldr (\x (mul, val) -> (mul * 10, x * mul + val)) (1,0) . map digitToInt

pNL s = putStrLn $ '\n' : s

int :: Parser Int
int = fmap strToInt $ some $ oneOf ['0'..'9']

parseTestMain :: IO ()
parseTestMain = do
  pNL "stop:"
  testParse2 stop
  pNL "one:"
  -- testParse2 one
  pNL "one':"
  testParse2 one'
  pNL "oneTwo:"
  testParse2 oneTwo
  pNL "oneTwo':"
  testParse2 oneTwo'

p123 :: Parser Int
p123 = fmap strToInt $ many $ oneOf "123"

str :: String -> Parser String
str [] = return []
str (x:xs) = (:) <$> char x <*> str xs

badFraction = "1/0"

alsoBad = "10"

shouldWork = "1/2"

shouldAlsoWork = "2/1"

badFractions = "1/0"

alsoBads = "10"

shouldWorks = "1/2"

shouldAlsoWorks = "2/1"

parserFraction :: (Monad m, TokenParsing m) => m Rational
parserFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail  "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testWithAttoParsec :: IO ()
testWithAttoParsec = do
  let attoP = parseOnly parserFraction
  print $ attoP badFraction
  print $ attoP alsoBad
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork

testWithTrifecta :: IO ()
testWithTrifecta = do
  let virtuousFraction' = parseString parserFraction mempty
  print $ virtuousFraction' badFractions 
  print $ virtuousFraction' alsoBads
  print $ virtuousFraction' shouldWorks
  print $ virtuousFraction' shouldAlsoWorks

onlyInteger :: Parser Integer
onlyInteger = do
  i <- integer
  eof
  return i

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = Left <$> integer <|> Right <$> some letter

parseNosmain = do
  let p f i = parseString f mempty i
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

eitherOr' :: String
eitherOr' = "\n123 \nabc \n456 \ndef"

parseCorrectly' :: Parser NumberOrString
parseCorrectly' = do
  many newline
  nos <- parseNos
  many space
  return nos

parseCorrectly :: Parser [NumberOrString]
parseCorrectly = do
  many parseCorrectly'


parseDecimalOrFraction :: Parser (Either Integer Rational)
parseDecimalOrFraction = try (Left <$> parseOnlyDecimal) <|> try (Right <$> parserFraction)
                         where parseOnlyDecimal ::  Parser Integer
                               parseOnlyDecimal = do
                                 i <- decimal
                                 eof
                                 return i

