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


splitAtPred :: (a -> Bool) -> [a] -> [[a]]
splitAtPred = accHelper []
    where accHelper []  pred []     = []
          accHelper acc pred []     = [acc]
          accHelper acc pred (x:xs)
                        | pred x    = [acc] ++ accHelper [] pred xs
                        | otherwise = accHelper (acc ++ [x]) pred xs

isNumeric :: String -> Bool
isNumeric str = and [all (\x -> or [(Data.Char.isDigit x), (x == '.')]) str,
                     (length $ filter (== '.') str) == 1]

tryReadNum :: String -> Maybe Double
tryReadNum nat | isNumeric nat = Just $ read nat
               | otherwise     = Nothing

tryReadBool :: String -> Maybe Bool
tryReadBool "True"  = Just True
tryReadBool "False" = Just False
tryReadBool x       = Nothing

data JsonParseError = NumberParseError
                    | BooleanParseError
                    | UnclosedStringError
                    | ArrayParseError JsonParseError
                    | ObjectParseError String JsonParseError
                    | ConfusingDelimiter Char

instance Show JsonParseError where
    show NumberParseError       = "Issue parsing a number"
    show BooleanParseError      = "Issue parsing a boolean"
    show UnclosedStringError    = "Issue: likely a string that isn't closed"
    show (ArrayParseError x)    = "In parsing array: " ++ (show x)
    show (ObjectParseError k x) = "In parsing object, at key: " ++ k ++ ", got error: " ++ (show x)
    show (ConfusingDelimiter t) = "Confused about delimiter: " ++ [t]

data TokenizeState = Start | InStr | Backslash

tokenizeJsonStr :: String -> Either JsonParseError [String]
tokenizeJsonStr = tokenizeState Start ""
    where tokenizeState Start     acc ('"':s)  = (tokenizeState InStr "" s) >>= (return . (acc :))
          tokenizeState Start     acc (':':s)  = (tokenizeState Start "" s) >>= (return . (acc :))
          tokenizeState Start     acc (',':s)  = (tokenizeState Start "" s) >>= (return . (acc :))
          tokenizeState Start     acc ('{':s)  = (tokenizeState Start "" s) >>= (return . (acc :))
          tokenizeState Start     acc ('}':s)  = (tokenizeState Start "" s) >>= (return . (acc :))
          tokenizeState Start     acc ('[':s)  = (tokenizeState Start "" s) >>= (return . (acc :))
          tokenizeState Start     acc (']':s)  = (tokenizeState Start "" s) >>= (return . (acc :))
          tokenizeState Start     acc (c:s)
                         | Data.Char.isSpace c = (tokenizeState Start "" s) >>= (return . (acc :))
                         | otherwise           = tokenizeState Start (acc ++ [c]) s
          tokenizeState Start     ""  ""       = Right []
          tokenizeState Start     acc ""       = Right (acc:[])
          tokenizeState InStr     acc ('"':s)  = (tokenizeState Start "" s) >>= (return . (acc :))
          tokenizeState InStr     acc ('\\':s) = tokenizeState Backslash (acc ++ "\\") s
          tokenizeState InStr     acc (c:s)    = tokenizeState InStr (acc ++ [c]) s
          tokenizeState InStr     acc ""       = Left UnclosedStringError
          tokenizeState Backslash acc (c:s)    = tokenizeState InStr (acc ++ [c]) s
          tokenizeState Backslash acc ""       = Left UnclosedStringError


