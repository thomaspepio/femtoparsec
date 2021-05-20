{-# LANGUAGE OverloadedStrings #-}
module SpecHelper
  ( module Test.Hspec
  , module Test.QuickCheck
  , Dummy(..)
  , dummyParser
  ) where

import qualified Data.Text                     as T
import           Femtoparsec
import           Test.Hspec
import           Test.QuickCheck

data Dummy = Dummy
  { someText   :: T.Text
  , someNumber :: Int
  }
  deriving (Show, Eq)

dummyParser :: Parser Dummy
dummyParser = do
  text <- till (T.pack "$") anyString
  char '$'
  num <- anyNum
  return $ Dummy text num

genPair :: a -> b -> Gen (a, b)
genPair left right = pure (left, right)

genTriple :: a -> b -> c -> Gen (a, b, c)
genTriple a b c = pure (a, b, c)

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genText :: Int -> Gen T.Text
genText length = do
  str <- listOf genChar
  return $ T.pack $ take length str
