{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Chapter24.ConfFile(
  parseAssignments,
  sectionEx'',
  parseINI,
  assignmentEx,
  sectionEx,
  p',
  testparseAssignment,
  maybeSuccess,
  testHeaderParsing,
  Header(Header),
  testCommentParsing,
  testSectionParsing,
  expected',
  testINIParsing,
  expected2,
  l'
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
parseAssignments = do 
  skipComments
  assignments <- some parseAssignment
  skipWhitespace
  return $ M.fromList assignments

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

sectionEx' :: ByteString
sectionEx' = ";ignore me\n[status]\nChris=Texas"

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

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a ) = Just a
maybeSuccess _ = Nothing

testparseAssignment :: Result (Name, Value)
testparseAssignment = parseByteString parseAssignment mempty assignmentEx

testHeaderParsing :: Result Header
testHeaderParsing = parseByteString parseHeader mempty headerEx

testCommentParsing :: ByteString -> Result Header
testCommentParsing = parseByteString (skipComments >> parseHeader) mempty

testSectionParsing :: Result Section
testSectionParsing = parseByteString parseSection mempty sectionEx'

expected' :: Maybe Section
expected' = Just (Section (Header "status") (M.fromList [("Chris", "Texas")]))

testINIParsing :: Result Config
testINIParsing = parseByteString parseINI mempty sectionEx

sectionValues = M.fromList [("alias", "claw"), ("host", "wikipedia")]

whatisitValues = M.fromList [("red", "int")]

expected2 = Just (Config (M.fromList [(Header "section", sectionValues), (Header "whatisit", whatisitValues)]))

p' :: Parser [Integer]
p' = some $ do
  i <- token $ some digit
  return $ read i

l' :: Parser [String]
l' = some $ do
  token $ some letter
