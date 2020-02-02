module Main where

import Lib
import Parser
import System.Environment
import Data.Char
import Data.String as Str
import Data.List as L
import Control.Monad
import Control.Applicative

genDecoder :: Decodeable -> String
genDecoder (Decodeable name t) = dname ++ " : JD.Decoder " ++ name ++ "\n" ++ dname ++ " = " ++ decode t 1
    where dname = firstToLower name ++ "Decoder"

genEncoder :: Decodeable -> String
genEncoder (Decodeable name t) = ename ++ " : " ++ name ++ " -> JE.Value" ++ "\n" ++ ename ++ " = " ++ encode t 0
    where ename = firstToLower name ++ "Encoder"

decode :: Type -> Int -> String
decode BoolT _       = "JD.bool"
decode IntT _        = "JD.int"
decode StringT _     = "JD.string"
decode FloatT _      = "JD.float"
decode (List t) d    = "(JD.list " ++ decode t d ++ ")"
decode (Record ps) d = "(JD.map" ++ ifThen (=="1") "" (show (length ps)) ++
                     " (\\" ++ foldl (\acc (Property n t) -> acc ++ n ++ " ") "" ps
                      ++ "-> {" ++ intercalate ", " (map (\(Property n t) -> n ++ "=" ++ n) ps) ++ "})"
                      ++ concatMap (\(Property n t) -> "\n" ++ concat (replicate d "    ") ++ "(JD.field \"" ++ n ++ "\" " ++ (decode t (d + 1)) ++ ")") ps
                      ++ ")"

--TODO
encode :: Type -> Int -> String
encode BoolT _       = "JE.bool"
encode IntT _        = "JE.int"
encode StringT _     = "JE.string"
encode FloatT _      = "JE.float"
encode (List t) d    = "(JE.list " ++ (encode t d) ++ ")"
encode (Record ps) d = "\\" ++ xname ++ " -> (JE.object [" ++ intercalate ", " (map (\(Property n t) -> "(" ++ n ++ ", " ++ encode t (d + 1) ++ " " ++ xname ++ "." ++ n ++ ")") ps) ++ "])"
    where xname = "x" ++ show d

main :: IO ()
main = do
    fileName <- (!!0) <$> getArgs
    content <- readFile fileName
    case parseDecodeables content of
        Left e -> putStrLn $ "Error: " ++ show e 
        Right decodeables -> do 
            print decodeables
            let generated = intercalate"\n\n\n" $ map (\x -> genDecoder x ++ "\n\n" ++ genEncoder x) $ decodeables
            writeFile ("Json_" ++ fileName) $ "module Json_" ++ L.takeWhile (/='.') fileName ++ " exposing(..)\nimport Json.Decode as JD\nimport Json.Encode as JE\n\n" ++ generated
            putStrLn $ "Written to: " ++ ("Json_" ++ fileName)
    
