{-# LANGUAGE OverloadedStrings #-}
module IO where

import Prelude hiding (readFile, putStrLn)
import Control.Monad (mzero)
import Data
import Data.Map.Lazy hiding (foldl, map, maximum, member, size)
import Data.Text.Lazy hiding (empty, foldl, map, maximum)
import Data.Aeson
import Data.ByteString.Lazy hiding (empty, foldl, map, maximum)

data JsonUnit = JsonUnit { member' :: [Cell], pivot' :: Cell } deriving Show

data JsonGame =
    JsonGame {
      gid' :: Int,
      units' :: [JsonUnit],
      width' :: Int,
      height' :: Int,
      filled' :: [Cell],
      sourceLength' :: Int,
      sourceSeeds' :: [Int]
    } deriving Show

data JsonSolution =
    JsonSolution {
      problemId :: Int,
      seed :: Int,
      tag :: Text,
      solution :: Text
    }

instance FromJSON Cell where
    parseJSON (Object v) = Cell <$> v .: "x" <*> v .: "y"
    parseJSON _ = mzero

instance FromJSON JsonUnit where
    parseJSON (Object v) = JsonUnit <$> v .: "members" <*> v .: "pivot"
    parseJSON _ = mzero

instance FromJSON JsonGame where
    parseJSON (Object v) =
        JsonGame
        <$> v .: "id"
        <*> v .: "units"
        <*> v .: "width"
        <*> v .: "height"
        <*> v .: "filled"
        <*> v .: "sourceLength"
        <*> v .: "sourceSeeds"
    parseJSON _ = mzero

instance ToJSON JsonSolution where
    toJSON solution' = object ["problemId" .= problemId solution',
                               "seed" .= seed solution',
                               "tag" .= tag solution',
                               "solution" .= solution solution']

readJsonGame :: FilePath -> IO (Maybe JsonGame)
readJsonGame file = readFile file >>= return . decode

printJsonSolution :: JsonSolution -> IO ()
printJsonSolution = putStrLn . encode

fromJsonGame :: JsonGame -> Game
fromJsonGame json =
    let size' = Size (width' json) (height' json)
    in Game {
         gid = gid' json,
         size = size',
         units = map (transformUnit $ width' json) $ units' json,
         board = foldl fillCell (emptyBoard size') $ filled' json,
         sourceLength = sourceLength' json,
         sourceSeeds = sourceSeeds' json }

transformUnit :: Int -> JsonUnit -> Unit
transformUnit width json =
    let w' = maximum (map x $ member' json) + 1
        h' = maximum (map y $ member' json) + 1
    in Unit {
         member = map (fromCell (pivot' json)) $ member' json,
         pivot = Cell ((width - w') `div` 2 + (x $ pivot' json))
                      (y $ pivot' json)
       }

fromCell :: Cell -> Cell -> Position
fromCell pivot cell = (x cell - x pivot, y cell - y pivot)

emptyBoard :: Size -> Board
emptyBoard size = foldl (\m y -> foldl (\m' x -> insert (Cell x y) Empty m') m [0..w size - 1]) empty [0..h size - 1]
