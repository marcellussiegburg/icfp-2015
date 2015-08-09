module Main where

import Data.Maybe (maybeToList)
import System.Environment

import IO

data Arguments =
    Arguments {
      files :: [FilePath],
      timeLimit :: Maybe Int,
      memoryLimit :: Maybe Int,
      phrases :: [String]
    } deriving Show

main :: IO ()
main = do
  parameters <- getArgs
  let arguments = parseParameters parameters
  json <- mapM readJsonGame $ files arguments
  let games = map fromJsonGame $ concat $ map maybeToList json
  putStrLn $ show games

parseParameters :: [String] -> Arguments
parseParameters = parseParametersHelper (Arguments [] Nothing Nothing [])

parseParametersHelper :: Arguments -> [String] -> Arguments
parseParametersHelper arguments [] = arguments
parseParametersHelper arguments (_ : []) = arguments -- ^ ignored if only one parameter
parseParametersHelper arguments (param : paramV : parameters) =
    case param of
      "-f" -> parseParametersHelper (arguments { files = paramV : files arguments }) parameters
      "-t" -> parseParametersHelper (arguments { timeLimit = Just $ read paramV }) parameters
      "-m" -> parseParametersHelper (arguments { memoryLimit = Just $ read paramV }) parameters
      "-p" -> parseParametersHelper (arguments { phrases = paramV : phrases arguments }) parameters
