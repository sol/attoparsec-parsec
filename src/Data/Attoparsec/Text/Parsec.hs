module Data.Attoparsec.Text.Parsec (
-- * Parser types
  Parser
-- , Result
-- , T.IResult(..)

-- * Running parsers
-- , parse
-- , feed
, parseOnly
-- , parseWith
-- , parseTest

-- ** Result conversion
-- , maybeResult
-- , eitherResult

-- * Combinators
, (<?>)
, try
, module Data.Attoparsec.Combinator

-- * Parsing individual characters
, char
, anyChar
-- , I.notChar
, satisfy
-- , I.satisfyWith
-- , I.skip

-- ** Special character parsers
-- , digit
-- , letter
-- , space

-- ** Character classes
, Attoparsec.inClass
, Attoparsec.notInClass

-- * Efficient string handling
, string
-- , stringCI
-- , skipSpace
, skipWhile
-- , I.scan
-- , I.take
, takeWhile
, takeWhile1
-- , I.takeTill

-- ** Consume all remaining input
-- , I.takeText
-- , I.takeLazyText

-- * Text parsing
, endOfLine
, Attoparsec.isEndOfLine
, Attoparsec.isHorizontalSpace

-- * Numeric parsers
-- , decimal
-- , hexadecimal
-- , signed
-- , double
-- , Number(..)
-- , number
-- , rational

-- * State observation and manipulation functions
, endOfInput
-- , I.atEnd
) where

import           Prelude hiding (takeWhile)

import           Data.Text   (Text)
import qualified Data.Text as Text

import           Control.Applicative
import           Text.Parsec.Text (Parser)
import qualified Text.Parsec as Parsec

import qualified Data.Attoparsec.Text as Attoparsec
import           Data.Attoparsec.Combinator

parseOnly :: Parser a -> Text -> Either String a
parseOnly p = either (Left . show) (Right) . Parsec.parse p ""

try :: Parser a -> Parser a
try = Parsec.try

(<?>) :: Parser a -> String -> Parser a
(<?>) = (Parsec.<?>)

char :: Char -> Parser Char
char = Parsec.char

endOfInput :: Parser ()
endOfInput = Parsec.eof

endOfLine :: Parser ()
endOfLine = Parsec.option '\r' (char '\r') >> char '\n' >> return ()


takeWhile :: (Char -> Bool) -> Parser Text
takeWhile p = Text.pack <$> many (satisfy p)

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = Text.pack <$> many1 (satisfy p)

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = Parsec.skipMany (satisfy p)

string :: String -> Parser Text
string s = Text.pack <$> Parsec.string s

anyChar :: Parser Char
anyChar = Parsec.anyChar

satisfy :: (Char -> Bool) -> Parser Char
satisfy = Parsec.satisfy
