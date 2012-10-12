# An Attoparsec compatibility layer for Parsec

`attoparsec-parsec` allows you to write parsers that can be compiled against
both [Attoparsec](http://hackage.haskell.org/package/attoparsec) and
[Parsec](http://hackage.haskell.org/package/parsec).

Example:

~~~ {.haskell}
{-# LANGUAGE CPP #-}
import           Prelude hiding (takeWhile)
import           Data.Text      (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

#ifdef USE_ATTOPARSEC
import           Data.Attoparsec.Text
#else
import           Data.Attoparsec.Text.Parsec
#endif

main :: IO ()
main = Text.interact (either error id . parseOnly removeDashes)

removeDashes :: Parser Text
removeDashes = Text.concat `fmap` sepBy text dash
  where
    text = takeWhile  (/= '-')
    dash = takeWhile1 (== '-')
~~~
