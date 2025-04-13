module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Control.Monad (when)
import Data.Aeson (Value(..), decode)
import Data.Aeson.Encode.Pretty (Indent(..), encodePretty', defConfig, confIndent)
import Data.Aeson.KeyMap (KeyMap, toList)
import Data.Aeson.Key (toString)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector, (!?), length)
import Data.Maybe (listToMaybe)
import System.IO (stderr, stdin, hPutStrLn)
import System.Environment (getArgs)

readJsonStream :: Int -> IO BSL.ByteString
readJsonStream chunkSize = do
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
  Just '.' -> drop 1 pth
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
  args <- getArgs
  let notmininal = "--mininal" `notElem` args
  jsonData <- readJsonStream 4096
  case decode jsonData of
    Just (Object o) -> do
      when notmininal $ do
        putStrLn "## title\n"
        putStrLn "> https://\n"
        putStrLn "*请求方法: *\n"
        putStrLn "认证方式: \n"
        putStrLn "**URL 参数:**\n"
        putStrLn "**JSON 回复:**\n"
      printObjectKV "" o
      when notmininal $ do
        putStrLn "**示例:**\n"
        putStrLn "```shell\ncurl -\n```\n"
        putStrLn "<details>\n<summary>查看响应示例:</summary>\n"
        putStrLn "```json"
        putStrLn $ unpack $ decodeUtf8 $ BSL.toStrict $ encodePretty' defConfig { confIndent = Spaces 2 } o
        putStrLn "```"
        putStrLn "</details>"
    Just val -> hPutStrLn stderr $ "Not Object root: " ++ getValueType val
    Nothing -> hPutStrLn stderr "Not valid JSON input"
