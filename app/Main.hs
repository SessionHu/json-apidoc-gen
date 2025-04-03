module Main where

import Data.Aeson (Value(Object, Array, String, Number, Bool, Null), decode)
import Data.Aeson.KeyMap (KeyMap, toList)
import Data.Aeson.Key (toString)
import Data.Vector (Vector, head, last)
import Data.ByteString.Lazy.Char8 (pack)

readJsonFromStdin :: IO (Maybe Value)
readJsonFromStdin = decode . pack <$> getContents

getArrayType :: Vector Value -> String
getArrayType arr =
  let fe = (getValueType $ Data.Vector.head arr
           ,getValueType $ Data.Vector.last arr)
  in
    if uncurry (==) fe
      then fst fe ++ "[]"
    else "any[]"

getValueType :: Value -> String
getValueType value = do
  case value of
    Object _ -> "object"
    Array ar -> getArrayType ar ++ "[]"
    String _ -> "string"
    Number _ -> "number"
    Bool _ -> "boolean"
    Null -> "null"

printObjectKV :: String -> KeyMap Value -> IO ()
printObjectKV pthr obj = do
  let lzt = toList obj
  let pth = if not (null pthr) && (Prelude.head pthr == '.') then tail pthr else pthr
  if pth == ""
    then putStrLn "根对象:"
  else if Prelude.last pth == ']'
    then putStrLn $ pth ++ "中的对象:"
  else putStrLn $ "`" ++ pth ++ "` 对象:"
  putStrLn "\n| 字段 | 类型 | 内容 | 备注 |"
  putStrLn   "| ---- | ---- | ---- | ---- |"
  mapM_ (\(k, v) -> do
    putStrLn $ "| " ++ toString k ++ " | " ++ getValueType v ++ " |  |  |"
    ) lzt
  putStrLn ""
  mapM_ (\(k, v) -> do
    case v of
      Object o -> printObjectKV (pth ++ "." ++ toString k) o
      Array arr -> do
        if getArrayType arr /= "object[]"
          then putStr ""
        else case Data.Vector.head arr of
          Object o -> printObjectKV (pth ++ "." ++ toString k ++ "[]")o
          _ -> putStr ""
      _ -> putStr ""
    ) lzt

main :: IO ()
main = do
  maybeValue <- readJsonFromStdin
  case maybeValue of
    Just v -> 
      case v of 
        Object o -> printObjectKV "" o
        _ -> putStrLn "Not valid input"
    _ -> putStrLn "Not valid input"
