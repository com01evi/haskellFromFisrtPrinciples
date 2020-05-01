{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Chapter24.ConfFile(
  parseAssignments,
  sectionEx'',
  parseINI,
  assignmentEx,
  sectionEx
)where

import Text.Trifecta
import Text.Parser.Token
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Control.Applicative
import Data.ByteString(ByteString)
import Data.Char(isAlpha)
import Data.Map(Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ

headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String deriving(Eq, Ord, Show)

parseHeader :: Parser Header
parseHeader = Header <$ char '[' <*> some letter <* char ']'

assignmentEx :: ByteString
assignmentEx = "woot=wiki\n hoge=homu"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  val <- some letter
  skipWhitespace
  return $ (name, val)

skipEol :: Parser ()
skipEol = skipMany newline

parseAssignments :: Parser Assignments
parseAssignments = M.fromList <$> some parseAssignment

commentEx :: ByteString
commentEx = "; last modified 1 Aplil 2001 by John Doe"

commentEx' :: ByteString 
commentEx' = "; blah\n; woot\n \n;hah"

sectionEx'' :: ByteString
sectionEx'' = [r|
  ; comment
  [section]
  host=wikipedia.org
  alias=claw

  [whatisit]
  red=intoothandclaw
  |]

sectionEx :: ByteString
sectionEx = ";comment\n[section]\nhost=wikipedia\nalias=claw\n\n[whatisit]\nred=int"

skipComments :: Parser ()
skipComments = skipMany ( do char ';' <|> char '#'
                             skipMany $ noneOf "\n"
                             skipEol)

data Section = Section Header Assignments deriving(Eq, Show)

newtype Config = Config (Map Header Assignments) deriving(Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> oneOf "\n")

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  skipWhitespace
  h <- parseHeader
  skipWhitespace
  assignments <- parseAssignments
  skipWhitespace
  return $ Section h assignments



rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseINI :: Parser Config
parseINI = do
  sections <- some parseSection
  let mapha = foldr rollup mempty sections
  return $ Config mapha
