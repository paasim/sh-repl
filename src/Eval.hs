module Eval
    ( replCommands
    ) where

import Control.Monad ( (>=>) )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import System.IO ( isEOF )
import System.Process

-- utils
splitOn :: Char -> String -> [String]
splitOn c' ""  = []
splitOn c' (c:rest) = if c == c'
  then "" : splitOn c' rest
  else joinToFirst c (splitOn c' rest) where
    joinToFirst c [] = [[c]]
    joinToFirst c (str:strs) = (c:str):strs

readProcess' :: FilePath -> [String] -> String -> IO String
readProcess' fp strs str = prefSndString <$> readProcessWithExitCode fp strs str where
  prefSndString (_, [], str) = str
  prefSndString (_, str, _)  = str
 
-- evaluating commands
data Command = Command { cmd :: FilePath, args :: [String], inputFrom :: Maybe Command } deriving (Eq, Show)

-- elements of outer lists are strings ending in ;
-- elements of inner lists are strings separated by | and 
-- reversed so that its easier create the data using
-- the inputFrom-fields
splitToCommandStrings :: String -> [[String]]
splitToCommandStrings = fmap (reverse . splitOn '|') . splitOn ';' . takeWhile ('#' /=)

-- traversal is needded to get the maybe outside from converting list to nonempty list
toCommandData :: String -> Maybe [[NonEmpty String]]
toCommandData = (traverse . traverse) (nonEmpty . words) . splitToCommandStrings

-- Commands processed from a list to get the composition right
commandDataToCommand :: [NonEmpty String] -> Maybe Command
commandDataToCommand []                   = Nothing
commandDataToCommand ((cmd :| args):rest) = Just $ Command cmd args (commandDataToCommand rest)

-- This is needed to handle lists of lists which arise from ;
stringToCommands :: String -> Maybe [Command]
stringToCommands = toCommandData >=> traverse commandDataToCommand

evalCommand :: Command -> IO String
evalCommand (Command cmd args Nothing) = readProcess' cmd args ""
evalCommand (Command cmd args (Just p)) = evalCommand p >>= readProcess' cmd args

evalCommands :: Maybe Int -> [Command] -> IO String
evalCommands Nothing  = fmap unlines . traverse evalCommand
-- the first lines . unlines is needed because newlines can be result
-- of multiple commands or stdout consisting of multiple lines
evalCommands (Just i) = fmap (unlines . trunc i . lines . unlines) . traverse evalCommand where
  trunc i l = if length l > i then take i l ++ ["..."] else l
  

evalString :: Maybe Int -> String -> IO String
evalString mi str = case stringToCommands str of
  (Just cmds) -> evalCommands mi cmds
  Nothing     -> return $ "Cannot parse the following as a valid onliner:\n" <> str

echoAndExec :: Maybe Int -> String -> String -> IO String
echoAndExec _ _ []  = return ""
echoAndExec mi prompt str = ((prompt <> " " <> str <> "\n") <>) <$> evalString mi str

replCommands :: Maybe Int -> String -> IO ()
replCommands mi prompt = do
  eof <- isEOF
  if eof
    then return ()
    else getLine >>= echoAndExec mi prompt >>= putStr >> replCommands mi prompt

