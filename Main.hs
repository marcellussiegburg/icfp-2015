{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Data.Maybe (maybeToList)
import Data.Map.Lazy hiding (concat, foldl, map, zip)
import Data.Text.Lazy hiding (concat, empty, foldl, map, zip)
import Data.Time.Clock
import System.Environment
import System.Posix.Resource
import System.Timeout

import Data
import IO
import Random
import Solver
import Translate

data Arguments =
    Arguments {
      files :: [FilePath],
      timeLimit :: Maybe Int,
      memoryLimit :: Maybe Integer,
      phrases :: [Text]
    } deriving Show

main :: IO ()
main = do
  parameters <- getArgs
  let arguments = parseParameters parameters
  maybe (return ())
        (\l -> setResourceLimit ResourceTotalMemory $ ResourceLimits (ResourceLimit (l - l `div` 10)) (ResourceLimit l)) $
        memoryLimit arguments
  json <- mapM readJsonGame $ files arguments
  let phrases' = foldl (\m p -> insert p (maybe [] id $ getPhraseCommands p) m) empty $ phrases arguments 
      games = map fromJsonGame $ concat $ map maybeToList json
      games' = concat $ map (\g -> map (\r -> (g, r)) $ sourceSeeds g) games
      randomUnits = map (\(g, r) -> (g, randomOrder (units g) r)) games'
      solutions = map (\(g, s) -> topDownPhraseRun (board g) s $ map snd $ toAscList phrases') randomUnits
      commandStrings = map (transformWithPhrases phrases') solutions
      gameCommandStrings = zip games' commandStrings
      jsonSolutions = map (\((g, r), c) -> JsonSolution {
                                             problemId = gid g,
                                             seed = r,
                                             tag = "",
                                             solution = c }) gameCommandStrings
  case timeLimit arguments of
    Nothing -> printJsonSolution jsonSolutions
    Just time -> do
         sols <- timeout time $ evaluate jsonSolutions
         maybe (return ()) printJsonSolution sols

parseParameters :: [String] -> Arguments
parseParameters = parseParametersHelper (Arguments [] Nothing Nothing [])

parseParametersHelper :: Arguments -> [String] -> Arguments
parseParametersHelper arguments [] = arguments
parseParametersHelper arguments (_ : []) = arguments -- ^ ignored if only one parameter
parseParametersHelper arguments (param : paramV : parameters) =
    case param of
      "-f" -> parseParametersHelper (arguments { files = paramV : files arguments }) parameters
      "-t" -> parseParametersHelper (arguments { timeLimit = Just $ read paramV - 1}) parameters
      "-m" -> parseParametersHelper (arguments { memoryLimit = Just $ read paramV * 1024 * 1024}) parameters
      "-p" -> parseParametersHelper (arguments { phrases = pack paramV : phrases arguments }) parameters




