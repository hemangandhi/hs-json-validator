module Json where

import qualified Data.Map
import qualified Data.Char

data Json = JsonString String
          | JsonNumber Double -- TODO: make extensible?
          | JsonNull
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

-- TODO: Replace with readMaybe
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

data JsonParseError = LiteralParseError
                    | UnclosedStringError
                    | ArrayParseError JsonParseError
                    | ObjectParseError String JsonParseError
                    | ConfusingDelimiter Char

instance Show JsonParseError where
    show LiteralParseError      = "Issue parsing a raw literal"
    show UnclosedStringError    = "Issue: likely a string that isn't closed"
    show (ArrayParseError x)    = "In parsing array: " ++ (show x)
    show (ObjectParseError k x) = "In parsing object, at key: " ++ k ++ ", got error: " ++ (show x)
    show (ConfusingDelimiter t) = "Confused about delimiter: " ++ [t]

data JsonTokenizeState = Start | InStr | Backslash

data JsonToken = OpenObject
               | OpenArray
               | StringToken String
               | Colon
               | Comma
               | Literal String
               | CloseObject
               | CloseArray

tokenizeJsonStr :: String -> Either JsonParseError [JsonToken]
tokenizeJsonStr = tokenizeState Start ""
    where tokenizeState Start     acc ('"':s)  = (tokenizeState InStr "" s) >>= (return . ((Literal acc) :))
          tokenizeState Start     acc (':':s)  = (tokenizeState Start "" s) >>= (return . ([Literal acc, Colon] ++))
          tokenizeState Start     acc (',':s)  = (tokenizeState Start "" s) >>= (return . ([Literal acc, Comma] ++))
          tokenizeState Start     acc ('{':s)  = (tokenizeState Start "" s)
                                                 >>= (return . ([Literal acc, OpenObject] ++))
          tokenizeState Start     acc ('}':s)  = (tokenizeState Start "" s)
                                                 >>= (return . ([Literal acc, CloseObject] ++))
          tokenizeState Start     acc ('[':s)  = (tokenizeState Start "" s)
                                                 >>= (return . ([Literal acc, OpenArray] ++))
          tokenizeState Start     acc (']':s)  = (tokenizeState Start "" s)
                                                 >>= (return . ([Literal acc, CloseArray] ++))
          tokenizeState Start     acc (c:s)
                         | Data.Char.isSpace c = (tokenizeState Start "" s) >>= (return . ((Literal acc) :))
                         | otherwise           = tokenizeState Start (acc ++ [c]) s
          tokenizeState Start     ""  ""       = Right []
          tokenizeState Start     acc ""       = Right ((Literal acc):[])
          tokenizeState InStr     acc ('"':s)  = (tokenizeState Start "" s) >>= (return . ((StringToken acc) :))
          tokenizeState InStr     acc ('\\':s) = tokenizeState Backslash (acc ++ "\\") s
          tokenizeState InStr     acc (c:s)    = tokenizeState InStr (acc ++ [c]) s
          tokenizeState InStr     acc ""       = Left UnclosedStringError
          tokenizeState Backslash acc (c:s)    = tokenizeState InStr (acc ++ [c]) s
          tokenizeState Backslash acc ""       = Left UnclosedStringError

parseArray :: ([JsonToken] -> Json -> Either JsonParseError Json) -> [Json]
                                                                  -> [JsonToken] -> Either JsonParseError Json
parseArray continuation arr tokens = parseWithContinuation tokens $ arrayCont arr continuation
    where arrayCont arr baseCont (Comma:ts)      newJson = parseArray baseCont (arr ++ [newJson]) ts
          arrayCont arr baseCont (CloseArray:ts) newJson = baseCont ts $ JsonArray $ arr ++ [newJson]
          arrayCont _   _        _               _       = Left $ ArrayParseError LiteralParseError

parseObject :: ([JsonToken] -> Json -> Either JsonParseError Json) -> (Data.Map.Map String Json)
                                                                   -> [JsonToken] -> Either JsonParseError Json
parseObject continuation obj = objectCont obj continuation
    where objectCont obj baseCont ((StringToken s):Colon:ts)       = parseWithContinuation ts $ commaCont obj baseCont s
          objectCont _   _        _                                = Left LiteralParseError
          commaCont obj baseCont key (Comma:ts)       value = parseObject baseCont (Data.Map.insert key value obj) ts
          commaCont obj baseCont key (CloseObject:ts) value = baseCont ts $ JsonObject $ Data.Map.insert key value obj
          commaCont _   _        k   _                _     = Left $ ObjectParseError k LiteralParseError

parseWithContinuation :: [JsonToken] -> ([JsonToken] -> Json -> Either JsonParseError Json)
                                     -> Either JsonParseError Json
parseWithContinuation ((StringToken s):xs)  continuation = continuation xs $ JsonString s
parseWithContinuation ((Literal "null"):xs) continuation = continuation xs $ JsonNull
parseWithContinuation ((Literal s):xs)      continuation = case tryReadBool s of
                                                             Just b  -> continuation xs $ JsonBoolean b
                                                             Nothing -> if   isNumeric s
                                                                        then continuation xs $ JsonNumber $ read s
                                                                        else Left LiteralParseError
parseWithContinuation (OpenArray:xs)        continuation = parseArray continuation [] xs
parseWithContinuation (OpenObject:xs)       continuation = parseObject continuation Data.Map.empty xs
parseWithContinuation _                     continuation = Left LiteralParseError

parseJsonTokens :: [JsonToken] -> Either JsonParseError Json
parseJsonTokens ts = parseWithContinuation ts expectEmpty
    where expectEmpty [] j = Right j
          expectEmpty ts j = Left LiteralParseError

-- TODO: find the appropriate dataclass for this.
tryReadJson s = tokenizeJsonStr s >>= parseJsonTokens
