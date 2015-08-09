module Data where

import Data.Map.Lazy hiding (size)

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
data State = Full | Empty deriving Show
type Board = Map Cell State

data Unit = Unit {
  pivot :: Cell,
  member :: [Position],
  unitSize :: Size
 } deriving Show

data Commands = Move | Turn
data Move = E | W | SE | SW
data Turn = Clockwise | CounterClockwise
