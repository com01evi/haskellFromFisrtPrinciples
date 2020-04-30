{-# LANGUAGE QuasiQuotes #-}

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
  testVirtuous,
  onlyInteger,
  parseNosmain,
  eitherOr,
  parseCorrectly,
  parseDecimalOrFraction
)where

import Text.Trifecta
import Text.Parser.Token
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Control.Applicative
import Data.Char
import Data.Ratio ((%))
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
parsePerson = Person <$ string "Person" <* space <*> integer <* comma <* comma <*> many (oneOf ['a'..'z']) <* (newline <|> return ' ')

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
int = fmap strToInt $ many $ oneOf ['0'..'9']

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

parserFraction :: Parser Rational
parserFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail  "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' = parseString parserFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork

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

parseCorrectly' :: Parser NumberOrString
parseCorrectly' = do
  skipMany newline
  parseNos

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

