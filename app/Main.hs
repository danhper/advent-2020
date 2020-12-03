{-# LANGUAGE TupleSections #-}

module Main where

import Runner (run)
import System.Environment (getArgs)
import Text.Read (readEither)

parseArgs :: IO (Either String (Int, Bool))
parseArgs = parseArg <$> getArgs
  where
    parseArg [] = Left "please provide a problem number\nusage: advent20 <n>\n"
    parseArg (x : "--sample" : _) = parseNum x True
    parseArg (x : _) = parseNum x False
    parseNum x isSample = (,isSample) <$> readEither x

main :: IO ()
main = parseArgs >>= either return (uncurry run) >>= putStr
