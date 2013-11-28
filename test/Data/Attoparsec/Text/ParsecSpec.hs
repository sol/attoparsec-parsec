{-# LANGUAGE OverloadedStrings #-}
module Data.Attoparsec.Text.ParsecSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Prelude hiding (take, takeWhile)
import           Control.Applicative
import qualified Data.Text as Text

import           Data.Attoparsec.Text.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "IsString instance for Parser" $ do
    it "recognizes a given string" $ do
      parseOnly "foo" "foo" `shouldBe` Right "foo"

    it "works without explicit type signature" $ do
      parseOnly ("foo" >> "bar") "foobar" `shouldBe` Right "bar"

  describe "atEnd" $ do
    it "parses an empty string to True" $ do
      parseOnly atEnd "" `shouldBe` Right True

    it "parses an non-empty string to False" $ do
      parseOnly atEnd "foo" `shouldBe` Right False

    it "does not consume any input" $ do
      parseOnly (atEnd *> takeWhile (const True) <* endOfInput) "foo" `shouldBe` Right "foo"

  describe "takeText" $ do
    it "consumes all remaining input" $ do
      parseOnly takeText "foo" `shouldBe` Right "foo"

  describe "takeLazyText" $ do
    it "consumes all remaining input" $ do
      parseOnly takeText "foo" `shouldBe` Right "foo"

  describe "take" $ do
    it "consumes exactly a given number n characters of input" $ do
      parseOnly (take 3) "foobar" `shouldBe` Right "foo"

    it "consumes nothing, if n is negative" $ do
      parseOnly ((,) <$> take (-3) <*> takeText) "foobar" `shouldBe` Right ("", "foobar")

  describe "stringCI" $ do
    it "ignores case" $ do
      parseOnly (stringCI "fooBAR") "FOObar" `shouldBe` Right "FOObar"

    it "does proper case folding" $ (const . pendingWith) "ignored" $ do
      parseOnly (stringCI "dass") "da\223" `shouldBe` Right "da\223"

  describe "decimal" $ do
    it "parses 23" $ do
      parseOnly decimal "23" `shouldBe` Right (23 :: Int)

    it "parses any postive decimal number" $ property $
      \(Positive n) -> parseOnly decimal (Text.pack $ show n) == Right (n :: Int)

  describe "hexadecimal" $ do
    it "parses 0xAB" $ do
      parseOnly hexadecimal "aB" `shouldBe` Right (171 :: Int)

  describe "Efficient string handling" $ do

    describe "string" $ do
      it "parses a given string" $ do
        parseOnly (string "foobar") "foobar" `shouldBe` Right "foobar"
