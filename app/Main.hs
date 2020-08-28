module Main where

import Eval
import System.Environment
import Text.Read

readInt :: String -> Maybe Int
readInt = readMaybe

parseArgs :: Maybe Int -> String -> String -> [String] -> IO ()
parseArgs mi prompt _ []                     = replCommands mi prompt
parseArgs mi _ pn ("--prompt":prompt:rest)   = parseArgs mi prompt pn rest
parseArgs _ prompt pn ("--max-lines":n:rest) = case readMaybe n of
  (Just n') -> parseArgs (Just n') prompt pn rest
  Nothing   -> putStrLn $ "error: " <> n <> "is not an integer"
parseArgs _ _ pn _                           =
  putStrLn $ "usage: " <> pn <> " [--max-lines n]"



main :: IO ()
main = do
  pn <- getProgName
  args <- getArgs
  parseArgs Nothing "$" pn args 

