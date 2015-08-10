module Data where

import Prelude hiding (lookup)
import Data.Map.Lazy (Map, insert, lookup)

data Game =
    Game {
      gid :: Int,
      size :: Size,
      units :: [Unit],
      board :: Board,
      sourceLength :: Int,
      sourceSeeds :: [Int]
    } deriving Show

data Cell = Cell { x :: Int, y :: Int} deriving (Show, Ord, Eq)
data Size = Size { w :: Int, h :: Int} deriving Show
type Position = (Int, Int)
data State = Full | Empty deriving (Show, Eq)
type Board = Map Cell State

data Unit = Unit {
  pivot :: Cell,
  member :: [Position]
 } deriving Show

data Command = M Move | T Turn deriving Show
data Move = E | W | SE | SW deriving Show
data Turn = Clockwise | CounterClockwise deriving Show

fromPosition :: Position -> Cell
fromPosition p = Cell (fst p) (snd p)

toPosition :: Cell -> Position
toPosition c = (x c, y c)

getCellOnBoard :: Cell -> Position -> Cell
getCellOnBoard pivot unitPosition =
    Cell (x pivot + fst unitPosition)
         (y pivot + snd unitPosition)

getCellsOnBoard :: Unit -> [Cell]
getCellsOnBoard unit =
    map (getCellOnBoard $ pivot unit) $ member unit

canPlaceCells :: Board -> [Cell] -> Bool
canPlaceCells board cells =
    foldl (\b c -> b && lookup c board == Just Empty) True cells

fillCell :: Board -> Cell -> Board
fillCell board cell = insert cell Full board

