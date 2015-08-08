{-# LANGUAGE OverloadedStrings #-}
module IO where

import Control.Monad (mzero)
import Data
import Data.Text
import Data.Aeson

data JsonUnit = JsonUnit { members :: [Cell], pivot :: Cell }

data JsonGame =
    JsonGame {
      gid :: Int,
      units :: [JsonUnit],
      width :: Int,
      height :: Int,
      filled :: [Cell],
      sourceLength :: Int,
      sourceSeeds :: [Int]
    }

data JsonSolution =
    JsonSolution {
      problemId :: Int,
      seed :: Int,
      tag :: Text,
      solution :: Text
    }

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
