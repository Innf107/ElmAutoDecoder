module Main where

import Lib
import Parser
import DecoderTypes
import System.Environment
import Data.Char
import Data.String as Str
import Data.List as L
import Control.Monad
import Control.Applicative
import Data.Bifunctor

genDecoder :: ([Decodeable]) -> String
genDecoder ds = concatMap (\(Decodeable n t) -> decodeTop n (toElm t) ++ "\n\n") ds

genEncoder :: ([Decodeable]) -> String
genEncoder ds = concatMap (\(Decodeable n t) -> encodeTop n (toElm t) ++ "\n\n") ds

toElm :: Type -> Decoder
toElm BoolT = Native "bool"
toElm IntT = Native "int"
toElm FloatT = Native "float"
toElm StringT = Native "string"
toElm (CustomT s) = Custom s
toElm (ListT t) = List $ toElm t
toElm (RecordT ps) = Record $ map (\(Property k v) -> (k, toElm v)) ps
toElm (ADT ts) = ADTD $ map (second (map toElm)) ts

decodeTop :: String -> Decoder -> String
decodeTop n d = "decode" ++ n ++ " : JD.Decoder " ++ n ++
               "\ndecode" ++ n ++ " = " ++ decode 1 d


decode :: Int -> Decoder -> String
decode i d = case d of
    Native s -> "JD." ++ s
    Custom s -> "decode" ++ s
    List d -> "(JD.list " ++ decode i d ++ ")"
    Record ps -> "(JD.map" ++ showOrEmpty (length ps) ++ genLambda (map fst ps) ++ "\n" ++ concatMap ((++"\n") . genField) ps ++ indent ++ ")"
    ADTD ts -> "(JD.field \"tag\" JD.string |> JD.andThen (\\t -> case t of\n"
      ++ concatMap ((++"\n") . genTag) ts
      ++ indent ++ indent ++ "_ -> JD.fail <| \"unexpected tag \" ++ t\n"
      ++ indent ++ "))"
    where
        indent = concat $ replicate i "    "
        genLambda :: [String] -> String
        genLambda ps = "(\\" ++ concatMap (++" ") ps ++ "-> {" ++ intercalate ", " (map (\p -> p ++ "=" ++ p) ps) ++ "})"
        genField :: (String, Decoder) -> String
        genField (n, d) = indent ++ "(JD.field \"" ++ n ++ "\" " ++ decode (i + 1) d ++ ")"
        genTag :: (String, [Decoder]) -> String
        genTag (t, ts) = case ts of
            [] -> indent ++ indent ++ "\"" ++ t ++ "\" -> JD.succeed " ++ t
            _  -> indent ++ indent ++ "\"" ++ t ++ "\" -> JD.map" ++ showOrEmpty (length ts) ++ " " ++ t ++ " "
                    ++ concatMapI (\i' d -> "(JD.field \"val" ++ show i' ++ "\" " ++ decode (i + 1) d ++ ")") ts
        showOrEmpty 1 = ""
        showOrEmpty x = show x

encodeTop :: String -> Decoder -> String
encodeTop n d = "encode" ++ n ++ " : " ++ n ++ " -> JE.Value" ++
              "\nencode" ++ n ++ " = " ++ encode 1 d

encode :: Int -> Decoder -> String
encode i d = case d of
    Native s -> "JE." ++ s
    Custom s -> "encode" ++ s
    List d -> "JE.list (" ++ encode (i + 1) d ++ ") "
    Record ps -> "(\\x" ++ show i ++ " -> JE.object [" ++ intercalate ", " (map (\(p, v) -> "(\"" ++ p ++ "\", " ++ encode (i + 1) v ++ " x" ++ show i ++ "." ++ p ++ ")") ps) ++ "])"
    ADTD ts -> "\\x" ++ show i ++ " -> case x" ++ show i ++ " of " ++ concatMap
        (\(t, ps) -> "\n" ++ indent ++ t ++ concatMapI (\i _ -> " val" ++ show i) ps
        ++ " -> JE.object [(\"tag\", JE.string \"" ++ t ++ "\")" ++ concatMapI
        (\i d -> ", (\"val" ++ show i ++ "\", " ++ encode (i + 1) d ++ " val" ++ show i ++ ")")
        ps ++
        "]")  ts
    where
        indent = concat $ replicate i "    "

main :: IO ()
main = do
    fileName <- (!!0) <$> getArgs
    content <- readFile fileName
    case parseDecodeables content of
        Left e -> putStrLn $ "Error: " ++ show e 
        Right (imports, decodeables) -> do
            let importStrs = concatMap (++"\n") imports
            let lastPart  = reverse (takeWhile (/='/') $ reverse fileName)
            let firstPart = dropWhileEnd (/='/') fileName
            let fileName' = firstPart ++ "Json_" ++ lastPart
            writeFile (fileName') $ "module Json_" ++ L.takeWhile (/='.') fileName
                ++ " exposing(..)\nimport Json.Decode as JD\nimport Json.Encode as JE\nimport "
                ++ (dropEnd 1 $ dropWhileEnd (/='.') fileName) ++ " exposing (..)\n\n" ++ importStrs ++ genDecoder decodeables
                ++ "\n\n" ++ genEncoder decodeables
            putStrLn $ "Written to: " ++ fileName'
    
