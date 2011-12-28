{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Spec (main, spec) where

import           Test.Hspec.ShouldBe

import           Prelude hiding (takeWhile)
import           Control.Applicative
import           Data.Attoparsec.Text.Parsec

main = hspec spec

spec = do

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
