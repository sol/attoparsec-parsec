{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- This module implements "Data.Attoparsec.Text" in terms of Parsec.  It can be
-- used to write parsers that can be compiled against both Attoparsec and
-- Parsec.
--
-- Differences from "Data.Attoparsec.Text":
--
-- * Incremental input is not supported.
--
-- * `A.satisfyWith`, `A.skip`, `A.scan`, and most of the numeric parsers are
-- not yet implemented.  Patches are gladly welcome!
--
-- * Parsec parsers (and hence the parsers provided here) do not automatically
--   backtrack on failing alternatives that consumed input.  With careful use
--   of `try` it is possible to write parsers that behave consistent across
--   Attoparsec and Parsec.  Read the next section for more on that.
--
-- A simple usage example is here: <https://github.com/sol/attoparsec-parsec#readme>
module Data.Attoparsec.Text.Parsec (

-- * Writing parsers that behave consistent across Attoparsec and Parsec
-- |
-- Some care is needed, so that parsers behave consistent across
-- Attoparsec and Parsec in regards to backtracking.  Attoparsec parsers always
-- backtrack on failure.  In contrast, a Parsec parser that fails after it has
-- consumed input will not automatically backtrack, but it can be turned into
-- backtracking parsers with `try`.
--
-- Here is an example that illustrates the difference.  The following parser
-- will fail under Parsec given an input of @\"for\"@:
--
-- >string "foo" <|> string "for"
--
-- The reason for its failure is that the first branch is a partial match, and
-- will consume the letters @\'f\'@ and @\'o\'@ before failing.  In Attoparsec,
-- the above parser will succeed on that input, because the failed first
-- branch will consume nothing.
--
-- The `try` function can be used to write parsers that behave consistent
-- across Attoparsec and Parsec.  Each alternative that may fail after
-- consuming input, has to be prefixed with @try@.  E.g. for the parser above
-- we would write:
--
-- >try (string "foo") <|> string "for"
--
-- For Parsec `try` enables backtracking, for Attoparsec it's just a
-- type-constrained version of `id` (see Attoparsec's `A.try`).




-- * Parser type
  Parser

-- * Running parsers
, parseOnly

-- * Combinators
, (<?>)
, try
, module Data.Attoparsec.Combinator

-- * Parsing individual characters
, char
, anyChar
, notChar
, satisfy
-- , satisfyWith
-- , skip

-- ** Special character parsers
, digit
, letter
, space

-- ** Character classes
, A.inClass
, A.notInClass

-- * Efficient string handling
, string
, stringCI
, skipSpace
, skipWhile
-- , scan
, take
, takeWhile
, takeWhile1
, takeTill

-- ** Consume all remaining input
, takeText
, takeLazyText

-- * Text parsing
, endOfLine
, A.isEndOfLine
, A.isHorizontalSpace

-- * Numeric parsers
, decimal
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
import           Data.Char
import           Data.String (IsString (..))
import           Data.Text   (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import           Control.Applicative
import           Control.Monad (replicateM)

import           Text.Parsec.Text (Parser)
import qualified Text.Parsec as Parsec

import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Combinator

parseOnly :: Parser a -> Text -> Either String a
parseOnly p = either (Left . show) (Right) . Parsec.parse p ""

instance (a ~ Text) => IsString (Parser a) where
  fromString = fmap Text.pack . Parsec.string

-- |
-- Name the parser, in case failure occurs.
--
-- See Parsec's documentation of `Parsec.<?>` for detailed semantics.
(<?>) :: Parser a
      -> String     -- ^ the name to use if parsing fails
      -> Parser a
(<?>) = (Parsec.<?>)
infix 0 <?>

-- |
-- Attempt a parse, and if it fails, rewind the input so that no input appears to have been consumed.
--
-- See Parsec's documentation of `Parsec.try` for detailed semantics.
try :: Parser a -> Parser a
try = Parsec.try

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
-- @s@. Returns the parsed string (i.e. @s@).
string :: Text -> Parser Text
string = fmap Text.pack . Parsec.string . Text.unpack

-- | Satisfy a literal string, ignoring case.
--
-- /Note/: No proper case folding is done, yet.  Currently @stringCI s@ is just
--
-- > char (toLower c) <|> char (toUpper c)
--
-- for each character of @s@.  The implementation from "Data.Attoparsec.Text"
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

-- | Parse and decode an unsigned decimal number.
decimal :: Integral a => Parser a
decimal = Text.foldl' step 0 `fmap` takeWhile1 isDecimal
  where step a c = a * 10 + fromIntegral (ord c - 48)

isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'

-- | Match only if all input has been consumed.
endOfInput :: Parser ()
endOfInput = Parsec.eof

-- | Return an indication of whether the end of input has been
-- reached.
atEnd :: Parser Bool
atEnd = (endOfInput *> pure True) <|> pure False
