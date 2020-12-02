module Main where

import Runner (run)
import System.Environment (getArgs)
import Text.Read (readEither)

getPbNumber :: IO (Either String Int)
getPbNumber = parseArg <$> getArgs
  where
    parseArg [] = Left "please provide a problem number\nusage: advent20 <n>\n"
    parseArg (x : _) = readEither x

main :: IO ()
main = getPbNumber >>= either return run >>= putStr
