module Data where

import Data.Map

type Cell = (Int, Int)
type Size = (Int, Int)
type Position = (Int, Int)
data State = Full | Empty
type Board = Map Cell State

data Unit = Unit { pivot :: Cell, member :: [Position] }

data Commands = Move | Turn
data Move = E | W | SE | SW
data Turn = Clockwise | CounterClockwise
