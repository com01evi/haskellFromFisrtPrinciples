module Chapter24.Parser(
hoge
)where

import Text.Trifecta
import Text.Parser.Token
import Text.Trifecta.Combinators
import Control.Applicative

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser a
one' = one >> stop

oneTwo = one >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser [Person] -> IO ()
testParse p = print $ parseString p mempty "123, , taro\n24, , akie"

parsePerson :: Parser Person 
parsePerson = Person <$> integer <* comma <* comma <*> many (oneOf ['a'..'z']) <* (newline <|> return ' ')

parsePeople :: Parser [Person]
parsePeople = many parsePerson

hoge = testParse parsePeople

data Person = Person { age :: Integer
                     , name :: String
                     }deriving(Eq, Show)
