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
, takeText
, takeLazyText

-- * Text parsing
, endOfLine
, isEndOfLine
, isHorizontalSpace

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
, atEnd
) where

import           Prelude hiding (takeWhile)
import           Data.Text   (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
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

-- | Consume all remaining input and return it as a single string.
takeText :: Parser Text
takeText = takeWhile (const True)

-- | Consume all remaining input and return it as a single string.
takeLazyText :: Parser L.Text
takeLazyText = L.pack `fmap` many (satisfy $ const True)

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: Parser ()
endOfLine = Parsec.option '\r' (char '\r') >> char '\n' >> return ()

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
isEndOfLine :: Char -> Bool
isEndOfLine = Attoparsec.isEndOfLine

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
isHorizontalSpace :: Char -> Bool
isHorizontalSpace = Attoparsec.isHorizontalSpace

-- | Match only if all input has been consumed.
endOfInput :: Parser ()
endOfInput = Parsec.eof

-- | Return an indication of whether the end of input has been
-- reached.
atEnd :: Parser Bool
atEnd = (endOfInput *> pure True) <|> pure False
