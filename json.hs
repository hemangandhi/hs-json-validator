
module Json where

import qualified Data.Map
import qualified Data.Char

data Json = JsonString String
          | JsonNumber Double -- TODO: make extensible?
          | JsonBoolean Bool
          | JsonArray [Json]
          | JsonObject (Data.Map.Map String Json)
          deriving (Eq)

getIndentationString :: Bool -> Int -> String
getIndentationString True  x = (replicate x ' ')
getIndentationString False _ = ""

showIndent :: Bool -> Int -> Json -> String
showIndent s i (JsonString  x) = (getIndentationString s i) ++ "\"" ++ (show x) ++ "\""
showIndent s i (JsonNumber  x) = (getIndentationString s i) ++ show x
showIndent s i (JsonBoolean x) = (getIndentationString s i) ++ show x
showIndent s i (JsonArray   x) = (getIndentationString s i) ++ "[\n"
                                                            ++ (unlines  -- want trailing \n
                                                                   $ map (\x -> (showIndent True (i + 4) x) ++ ",") x)
                                                            ++ "]"
showIndent s i (JsonObject  x) = (getIndentationString s i) ++ "{\n"
                                                            ++ (Data.Map.foldrWithKey (getMapStr s (i + 4)) "" x)
                                                            ++ "\n}"
    where getMapStr s i k (JsonArray x)  acc = acc ++ "\n"
                                                   ++ (getIndentationString s i)
                                                   ++ "\"" ++ k ++ "\": "
                                                   ++ (showIndent True (i + 4) (JsonArray x))
          getMapStr s i k (JsonObject x) acc = acc ++ "\n"
                                                   ++ (getIndentationString s i)
                                                   ++ "\"" ++ k ++ "\": "
                                                   ++ (showIndent True (i + 4) (JsonObject x))
          getMapStr s i k x              acc = acc ++ "\n"
                                                   ++ (getIndentationString s i)
                                                   ++ "\"" ++ k ++ "\": "
                                                   ++ (showIndent False (i + 4) x)

instance Show Json where show = showIndent False 0

splitAtPred :: a => (a -> Bool) -> [a] -> [[a]]
splitAtPred = accHelper []
where accHelper acc pred []     = [acc]
      accHelper acc pred (x:xs)
                    | pred x    = [acc] ++ accHelper [] pred xs
                    | otherwise = accHelper (acc ++ x) pred xs

isNumeric :: String -> Bool
isNumeric str = and [all (\x -> or [(Data.Char.isDigit x), (x == '.')]) str,
                     (length $ filter (== '.') str) == 1]

tryReadNum :: String -> Maybe Double
tryReadNum nat | isNumeric nat = Just $ read nat
               | otherwise     = Nothing

data JsonParseError = JsonNumberParseError
                    | BooleanParseError
                    | ArrayParseError JsonParseError
                    | ObjectParseError String JsonParseError
                    | ConfusingDelimiter Char


