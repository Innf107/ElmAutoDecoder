module Parser where

import Lib
import Text.Parsec(ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Char
import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Data.Char
import Text.Parsec.String.Combinator
import Control.Applicative hiding (optional)

data Type = BoolT
          | IntT
          | StringT
          | FloatT
          | ListT Type
          | CustomT String
          | RecordT [Property]
          | ADT [(String, [Type])]
          deriving Show

data Property = Property String Type
    deriving Show

data Decodeable = Decodeable String Type deriving Show

parseDecodeables :: String -> Either ParseError ([String],[Decodeable])
parseDecodeables = regularParse $ decodeables

decodeables :: Parser ([String],[Decodeable])
decodeables = do 
    spaces
    optional moduleDef
    spaces
    is <- many importP
    spaces
    ds <- many decodeable
    return (is, ds)

moduleDef :: Parser ()
moduleDef = do
    string "module"
    spaces
    many1 $ noneOf " \t\n"
    spaces
    string "exposing"
    spaces
    between (string "(") (string ")") (many $ noneOf ")")
    return ()

importP :: Parser String
importP = do
    string "import"
    spaces
    x <- many1 $ noneOf "\n"
    spaces
    return $ "import " ++ x ++ "\n"

decodeable :: Parser Decodeable
decodeable = do
    string "type"
    spaces
    alias <|> adt
    where
        alias = do
            string "alias"
            spaces
            name <- alphaNumStr
            spaces
            string "="
            spaces
            t <- typeT
            spaces
            return $ Decodeable name t
        adt = do
            name <- alphaNumStr
            spaces
            string "="
            spaces
            tags <- tag `sepBy1` (string "|")
            spaces
            return $ Decodeable name (ADT tags)
        tag = do
            spaces
            t <- alphaNumStr
            spaces
            ps <- many (typeT <* spaces)
            spaces
            return (t, ps)

typeT :: Parser Type 
typeT = intT <|> boolT <|> stringT <|> floatT <|> listT <|> recordT <|> customT

intT :: Parser Type
intT = string "Int" >> return IntT
boolT = string "Bool" >> return BoolT
stringT = string "String" >> return StringT
floatT = string "Float" >> return FloatT
listT = do
    string "List"
    spaces
    t <- typeT
    return $ ListT t
recordT = do
    string "{"
    spaces
    ps <- many property
    spaces
    string "}"
    return $ RecordT ps
customT = do
    c <- satisfy isUpper
    rest <- many alphaNum
    return $ CustomT $ c:rest

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
