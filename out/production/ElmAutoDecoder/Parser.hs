module Parser where

import Lib
import Text.Parsec(ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Char (anyChar)
import Text.Parsec.String.Char
import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Data.Char
import Text.Parsec.String.Combinator (many1)
import Control.Applicative

data Type = BoolT
          | IntT
          | StringT
          | FloatT
          | List Type
          | Record [Property]
          deriving Show

data Property = Property String Type
    deriving Show

data Decodeable = Decodeable String Type deriving Show

parseDecodeables :: String -> Either ParseError [Decodeable]
parseDecodeables = regularParse $ decodeables

decodeables :: Parser [Decodeable]
decodeables = do 
    spaces
    optional $ do
        string "module"
        spaces
        alphaNumStr
        spaces
        string "exposing"
        spaces
        string "("
        anyOf $ alphaNumChars ++ "._,\n "
        string ")"
    spaces
    many decodeable

decodeable :: Parser Decodeable
decodeable = do 
    string "type"
    spaces
    string "alias"
    spaces 
    name <- alphaNumStr
    spaces 
    string "="
    spaces
    t <- typeT
    spaces
    return $ Decodeable name t

typeT :: Parser Type 
typeT = intT <|> boolT <|> stringT <|> floatT <|> listT <|> recordT

intT :: Parser Type
intT = string "Int" >> return IntT
boolT = string "Bool" >> return BoolT
stringT = string "String" >> return StringT
floatT = string "Float" >> return FloatT
listT = do
    string "List"
    spaces
    t <- typeT
    return $ List t
recordT = do
    string "{"
    spaces
    ps <- many property
    spaces
    string "}"
    return $ Record ps 
    
property :: Parser Property
property = do 
    name <- alphaNumStr
    spaces
    string ":"
    spaces
    t <- typeT
    spaces 
    optional $ string ","
    spaces
    return $ Property name t
    