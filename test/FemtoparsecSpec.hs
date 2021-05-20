{-# LANGUAGE OverloadedStrings #-}
module FemtoparsecSpec where

import qualified Data.Text                     as T
import           Femtoparsec
import           SpecHelper

spec :: Spec
spec = describe "Femtoparsec Spec" $ do

    describe "error messages" $ do
        it "tells you what string it can't parse" $ do
            parse "foo" (string "bar")
                `shouldBe` Left "Unable to parse bar from foo"

        it "tells you what number it can't parse" $ do
            parse "123" (num 456) `shouldBe` Left "Unable to parse 456 from 123"

    describe "parsing to basic data types" $ do
        it "parses to a specific string" $ do
            parse "foo" (string "foo") `shouldBe` Right ("foo", "")

        it "parses to a specific string and yields the rest of the source" $ do
            parse "foobar" (string "foo") `shouldBe` Right ("foo", "bar")

        it "parses to any string" $ do
            parse "foobar" anyString `shouldBe` Right ("foobar", "")

        it "parses to a char" $ do
            parse "a" (char 'a') `shouldBe` Right ('a', "")

        it "parses to an int" $ do
            parse "123" (num 123) `shouldBe` Right (123, "")

        it "parses to an int and yields the rest of the source" $ do
            parse "123foobar" (num 123) `shouldBe` Right (123, "foobar")

        it "parses to any number" $ do
            parse "123" anyNum `shouldBe` Right (123, "")

    describe "till combinator" $ do
        it "parses to a string until a point" $ do
            parse "foo$bar" (till (T.pack "$") (string "foo"))
                `shouldBe` Right ("foo", "$bar")

        it "parses to an int until a point" $ do
            parse "123$foo$bar" (till (T.pack "$") (num 123))
                `shouldBe` Right (123, "$foo$bar")

        it "parses any string until a point" $ do
            parse "foo$bar$baz" (till (T.pack "$") anyString)
                `shouldBe` Right ("foo", "$bar$baz")

        it "parses any int until a point" $ do
            parse "123$bar$baz" (till (T.pack "$") anyNum)
                `shouldBe` Right (123, "$bar$baz")

    describe "parsing a \"complex\" data type" $ do
        it "parsing needs to be monadic to support parsing each field" $ do
            parse "foo$123" dummyParser
                `shouldBe` Right (Dummy (T.pack "foo") 123, "")
