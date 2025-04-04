module Main where

import Data.Aeson (Value(..), decode)
import Data.Aeson.KeyMap (KeyMap, toList)
import Data.Aeson.Key (toString)
import Data.ByteString (ByteString, hGetSome, null)
import Data.ByteString.Lazy (fromStrict, concat)
import Data.Vector (Vector, head, last)
import System.IO (stdin, hSetBinaryMode)

readChunks :: Int -> IO [ByteString]
readChunks chunkSize = do
  hSetBinaryMode stdin True
  let loop = do
        chunk <- hGetSome stdin chunkSize
        if Data.ByteString.null chunk
          then return []
          else (chunk :) <$> loop
  loop

getArrayType :: Vector Value -> String
getArrayType arr = do
  if Prelude.null arr
    then "unknown[]"
  else do
    let f = getValueType $ Data.Vector.head arr
    let s = getValueType $ Data.Vector.last arr
    if f == s
      then f ++ "[]"
    else "any[]"

getValueType :: Value -> String
getValueType value = case value of
    Object _ -> "object"
    Array ar -> getArrayType ar
    String _ -> "string"
    Number _ -> "number"
    Bool _ -> "boolean"
    Null -> "null"

printObjectKV :: String -> KeyMap Value -> IO ()
printObjectKV pthr obj = do
  let lzt = toList obj
  let pth = if not (Prelude.null pthr) && (Prelude.head pthr == '.') then tail pthr else pthr
  if pth == ""
    then putStrLn "根对象:"
  else if Prelude.last pth == ']'
    then putStrLn $ "`" ++ pth ++ "`中的对象:"
  else putStrLn $ "`" ++ pth ++ "` 对象:"
  putStrLn "\n| 字段 | 类型 | 内容 | 备注 |"
  putStrLn   "| ---- | ---- | ---- | ---- |"
  mapM_ (\(k, v) -> do
    putStrLn $ "| " ++ toString k ++ " | " ++ getValueType v ++ " |  |  |"
    ) lzt
  putStrLn ""
  mapM_ (\(k, v) -> handleNestedObject (pth ++ "." ++ toString k) v) lzt

handleNestedObject :: String -> Value -> IO ()
handleNestedObject pth v = case v of
  Object o -> printObjectKV pth o
  Array arr -> do
    if getArrayType arr /= "object[]"
      then putStr ""
    else case Data.Vector.head arr of
      Object o -> printObjectKV (pth ++ "[]") o
      _ -> putStr ""
  _ -> putStr ""

main :: IO ()
main = do
  chunks <- readChunks 4096
  case decode $ Data.ByteString.Lazy.concat $ map fromStrict chunks of
    Just (Object o) -> printObjectKV "" o
    _ -> putStrLn "Not valid input"
