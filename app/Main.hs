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

genDecoder :: ([String],[Decodeable]) -> String
genDecoder (imports, ds) = concatMap (++"\n") imports
        ++ concatMap (\(Decodeable n t) -> fromElmTop n (decode t) ++ "\n\n") ds

decode :: Type -> Decoder
decode BoolT = Native "bool"
decode IntT = Native "int"
decode FloatT = Native "float"
decode StringT = Native "string"
decode (CustomT s) = Custom s
decode (ListT t) = List $ decode t
decode (RecordT ps) = Record $ map (\(Property k v) -> (k, decode v)) ps
decode (ADT ts) = ADTD $ map (second (map decode)) ts

fromElmTop :: String -> Decoder -> String
fromElmTop n d = "decode" ++ n ++ " : JD.Decoder " ++ n ++
               "\ndecode" ++ n ++ " = " ++ fromElm 1 d


fromElm :: Int -> Decoder -> String
fromElm i d = case d of
    Native s -> "JD." ++ s
    Custom s -> "decode" ++ s
    List d -> "(JD.list " ++ fromElm i d ++ ")"
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
        genField (n, d) = indent ++ "(JD.field \"" ++ n ++ "\" " ++ fromElm (i + 1) d ++ ")"
        genTag :: (String, [Decoder]) -> String
        genTag (t, ts) = case ts of
            [] -> indent ++ indent ++ "\"" ++ t ++ "\" -> JD.succeed " ++ t
            _  -> indent ++ indent ++ "\"" ++ t ++ "\" -> JD.map" ++ showOrEmpty (length ts) ++ " " ++ t ++ " "
                    ++ concatMapI (\i' d -> "(JD.field \"val" ++ show i' ++ "\" " ++ fromElm (i + 1) d ++ ")") ts
        showOrEmpty 1 = ""
        showOrEmpty x = show x

main :: IO ()
main = do
    fileName <- (!!0) <$> getArgs
    content <- readFile fileName
    case parseDecodeables content of
        Left e -> putStrLn $ "Error: " ++ show e 
        Right decodeables -> do 
            print decodeables
            let generated = genDecoder decodeables
            let lastPart  = reverse (takeWhile (/='/') $ reverse fileName)
            let firstPart = dropWhileEnd (/='/') fileName
            let fileName' = firstPart ++ "Json_" ++ lastPart
            writeFile (fileName') $ "module Json_" ++ L.takeWhile (/='.') fileName ++ " exposing(..)\nimport Json.Decode as JD\nimport Json.Encode as JE\nimport " ++ (dropEnd 1 $ dropWhileEnd (/='.') fileName) ++ " exposing (..)\n\n" ++ generated
            putStrLn $ "Written to: " ++ fileName'
    
