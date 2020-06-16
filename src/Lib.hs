{-# LANGUAGE LambdaCase #-}
module Lib where

import Text.Parsec.String (Parser)
import Text.Parsec.String.Char (anyChar)
import Text.Parsec.String.Char
import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Data.Char
import Text.Parsec.String.Combinator (many1)
import Control.Applicative

alphaNumStr :: Parser String
alphaNumStr = many $ alphaNum

anyOf :: [Char] -> Parser String
anyOf = many . oneOf 

alphaNumChars :: [Char]
alphaNumChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']


firstToLower :: String -> String 
firstToLower [] = []
firstToLower (c:cs) = (toLower c):cs


mapIndexed :: Num b => (a -> b -> c) -> [a] -> [c]
mapIndexed f xs = mapIndexedInner f 0 xs
    where mapIndexedInner :: Num b => (a -> b -> c) -> b -> [a] -> [c]
          mapIndexedInner _ _ [] = []
          mapIndexedInner f i (x:xs) = (f x i):(mapIndexedInner f (i + 1) xs)
          
ifThen :: (a -> Bool) -> a -> a -> a
ifThen f y x
    | f x = y
    | otherwise = x

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs

concatMapI :: (Int -> a -> [b]) -> [a] -> [b]
concatMapI = concatMapInner 0
    where
        concatMapInner _ _ [] = []
        concatMapInner i f (x:xs) = f i x ++ concatMapInner (i + 1) f xs 
        
        