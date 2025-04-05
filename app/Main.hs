module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Aeson (Value(..), decode)
import Data.Aeson.KeyMap (KeyMap, toList)
import Data.Aeson.Key (toString)
import Data.Vector (Vector, (!?), length)
import Data.Maybe (listToMaybe)
import System.IO (stdin, hSetBinaryMode)

readJsonStream :: Int -> IO BSL.ByteString
readJsonStream chunkSize = do
  hSetBinaryMode stdin True
  let readChunks = do
        chunk <- BS.hGetSome stdin chunkSize
        if BS.null chunk
          then return []
          else (chunk :) <$> readChunks
  BSL.fromChunks <$> readChunks

safeHead, safeLast :: Vector a -> Maybe a
safeHead vec = vec !? 0
safeLast vec = vec !? (Data.Vector.length vec - 1)

getArrayType :: Vector Value -> String
getArrayType arr
  | null arr = "unknown[]"
  | otherwise =
    let types = [maybe "unknown" getValueType $ safeHead arr
                ,maybe "unknown" getValueType $ safeLast arr]
        headType = maybe "any" getValueType $ safeHead arr
        allSame = all (== headType) types
    in
      if allSame
        then headType ++ "[]"
      else "any[]"

getValueType :: Value -> String
getValueType v = case v of
  Object _ -> "object"
  Array ar -> getArrayType ar
  String _ -> "string"
  Number _ -> "number"
  Bool _ -> "boolean"
  Null -> "null"

sanitizePath :: String -> String
sanitizePath pth = case listToMaybe pth of
  Just '.' -> tail pth
  _ -> pth

printObjectKV :: String -> KeyMap Value -> IO ()
printObjectKV path obj = do
  let entries = toList obj
      cleanPath = sanitizePath path
      header = case (cleanPath, listToMaybe cleanPath) of
        ("", _) -> "根对象:"
        (p, Just ']') -> "`" ++ p ++ "`中的对象:"
        (p, _) -> "`" ++ p ++ "` 对象:"
  putStrLn header
  putStrLn "\n| 字段 | 类型 | 内容 | 备注 |"
  putStrLn   "| ---- | ---- | ---- | ---- |"
  mapM_ (\(k, v) ->
    putStrLn $ "| " ++ toString k ++ " | " ++ getValueType v ++ " |  |  |"
    ) entries
  putStrLn ""
  mapM_ (\(k, v) -> handleNestedObject (cleanPath ++ "." ++ toString k) v) entries

handleNestedObject :: String -> Value -> IO ()
handleNestedObject path v = case v of
  Object o -> printObjectKV path o
  Array arr -> case safeHead arr of
    Just (Object o) -> printObjectKV (path ++ "[]") o
    _ -> pure ()
  _ -> pure ()

main :: IO ()
main = do
  jsonData <- readJsonStream 4096
  case decode jsonData of
    Just (Object o) -> printObjectKV "" o
    Just val -> putStrLn $ "Not Object root: " ++ getValueType val
    Nothing -> putStrLn "Not valid JSON input"
