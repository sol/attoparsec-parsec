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
