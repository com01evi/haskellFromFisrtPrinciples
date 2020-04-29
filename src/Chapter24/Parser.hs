module Chapter24.Parser(
  hoge,
  strToInt,
  testParse2,
  oneTwoThree,
  parseTestMain,
  int,
  p123,
  one
)where

import Text.Trifecta
import Text.Parser.Token
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Control.Applicative
import Data.Char

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
