{-# LANGUAGE OverloadedStrings #-}

module Chapter24.PolymorphicParser(
  backtrackMain
)where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString(ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

noBackParse :: (Monad f, CharParsing f) => f Char
noBackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> try (char '1' >> char '3')

hoge :: (Monad f, CharParsing f, TokenParsing f) => f [String]
hoge = some $ token (some digit)

backtrackMain :: IO ()
backtrackMain = do
  trifP noBackParse "13"
  trifP hoge "13 12 13"
  parsecP noBackParse "13"
  parsecP tryParse "13"
  attoP noBackParse "13"
  attoP tryParse "13"
