{-# LANGUAGE OverloadedStrings #-}
module Femtoparsec
    ( Parser(..)
    , ParseError
    , parse
    , string
    , anyString
    , char
    , num
    , anyNum
    , till
    ) where

import           Control.Monad.State.Lazy
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Text.Read                     as TR

type ParseError = T.Text
type Parser a = StateT T.Text (Either ParseError) a

parse :: T.Text -> Parser a -> Either ParseError (a, T.Text)
parse source parser = runStateT parser source

string :: T.Text -> Parser T.Text
string toMatch = textual toMatch toMatch

anyString :: Parser T.Text
anyString = do
    source <- get
    put nothingLeft
    return source

char :: Char -> Parser Char
char toMatch = textual toMatch (T.pack [toMatch])

num :: Int -> Parser Int
num toMatch = textual toMatch (T.pack . show $ toMatch)

anyNum :: Parser Int
anyNum = do
    source <- get
    case TR.readMaybe (T.unpack source) of
        Nothing ->
            lift $ Left $ T.pack "Unable to parse any number from " <> source
        Just n -> put nothingLeft >> pure n

till :: T.Text -> Parser a -> Parser a
till stopsAt parser = do
    source <- get
    let splitted = splitOn (T.unpack stopsAt) (T.unpack source)
    if length splitted > 0
        then
            let toParse = T.pack $ head splitted
                rest = T.pack $ concatMap (T.unpack stopsAt <>) $ tail splitted
            in  put toParse >> parser <* put rest
        else lift
            (  Left
            $  T.pack "Unable to parse until "
            <> stopsAt
            <> T.pack " from "
            <> source
            )

textual :: Show a => a -> T.Text -> Parser a
textual toMatch toMatchPacked = do
    source <- get
    if toMatchPacked `T.isPrefixOf` source
        then put (fromJust $ T.stripPrefix toMatchPacked source)
            >> lift (Right toMatch)
        else lift $ Left $ parseError toMatchPacked source

parseError :: T.Text -> T.Text -> ParseError
parseError toMatch source =
    T.pack "Unable to parse " <> toMatch <> T.pack " from " <> source

nothingLeft :: T.Text
nothingLeft = T.pack ""
