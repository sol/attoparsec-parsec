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
, notChar
, satisfy
-- , I.satisfyWith
-- , I.skip

-- ** Special character parsers
, digit
, letter
, space

-- ** Character classes
, Attoparsec.inClass
, Attoparsec.notInClass

-- * Efficient string handling
, string
, stringCI
, skipSpace
, skipWhile
-- , I.scan
, take
, takeWhile
, takeWhile1
, takeTill

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

import           Prelude hiding (take, takeWhile)
import           Data.Char (toLower, toUpper)
import           Data.Text   (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import           Control.Applicative
import           Control.Monad (replicateM)

import           Text.Parsec.Text (Parser)
import qualified Text.Parsec as Parsec

import qualified Data.Attoparsec.Text as Attoparsec
import           Data.Attoparsec.Combinator

parseOnly :: Parser a -> Text -> Either String a
parseOnly p = either (Left . show) (Right) . Parsec.parse p ""

try :: Parser a -> Parser a
try = Parsec.try

infix 0 <?>
(<?>) :: Parser a -> String -> Parser a
(<?>) = (Parsec.<?>)

-- | Match a specific character.
char :: Char -> Parser Char
char = Parsec.char

-- | Match any character.
anyChar :: Parser Char
anyChar = Parsec.anyChar

-- | Match any character except the given one.
notChar :: Char -> Parser Char
notChar c = satisfy (/= c) <?> "not " ++ show c

-- | The parser @satisfy p@ succeeds for any character for which the
-- predicate @p@ returns 'True'. Returns the character that is
-- actually parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
satisfy :: (Char -> Bool) -> Parser Char
satisfy = Parsec.satisfy

-- | Parse a single digit, as recognised by 'isDigit'.
digit :: Parser Char
digit = Parsec.digit

-- | Parse a letter, as recognised by 'isAlpha'.
letter :: Parser Char
letter = Parsec.letter

-- | Parse a space character, as recognised by 'isSpace'.
space :: Parser Char
space = Parsec.space

-- | @string s@ parses a sequence of characters that identically match
-- @s@. Returns the parsed string (i.e. @s@).  This parser consumes no
-- input if it fails (even if a partial match).
--
-- /Note/: The behaviour of this parser is different to that of the
-- similarly-named parser in Parsec, as this one is all-or-nothing.
-- To illustrate the difference, the following parser will fail under
-- Parsec given an input of @"for"@:
--
-- >string "foo" <|> string "for"
--
-- The reason for its failure is that that the first branch is a
-- partial match, and will consume the letters @\'f\'@ and @\'o\'@
-- before failing.  In Attoparsec, the above parser will /succeed/ on
-- that input, because the failed first branch will consume nothing.
string :: String -> Parser Text
string s = Text.pack <$> Parsec.string s

-- | Satisfy a literal string, ignoring case.
--
-- NOTE: No proper case folding is done, yet.  Currently @stringCI s@ is just
--
-- > char (toLower c) <|> char (toUpper c)
--
-- for each character of @s@.  The implementation from @Data.Attoparsec.Text@
-- tries to do proper case folding, but is actually buggy (see
-- <https://github.com/bos/attoparsec/issues/6>).  As long as you deal with
-- characters from the ASCII range, both implementations should be fine.
stringCI :: Text -> Parser Text
stringCI = fmap Text.pack . sequence . map f . Text.unpack
  where
    f c = char (toLower c) <|> char (toUpper c)

-- | Skip over white space.
skipSpace :: Parser ()
skipSpace = Parsec.spaces

-- | Skip past input for as long as the predicate returns 'True'.
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = Parsec.skipMany (satisfy p)

-- | Consume exactly @n@ characters of input.
take :: Int -> Parser Text
take n = Text.pack <$> replicateM n anyChar

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'False' on the first character of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeWhile :: (Char -> Bool) -> Parser Text
takeWhile p = Text.pack <$> many (satisfy p)

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one
-- character of input: it will fail if the predicate never returns
-- 'True' or if there is no input left.
takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = Text.pack <$> many1 (satisfy p)

-- | Consume input as long as the predicate returns 'False'
-- (i.e. until it returns 'True'), and return the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'True' on the first character of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeTill :: (Char -> Bool) -> Parser Text
takeTill p = takeWhile (not . p)

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
