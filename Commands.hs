module Commands where

import Data

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving Show

tryPerformCommands :: Board -> Unit -> [Command] -> Maybe Board
tryPerformCommands board unit [] = Just board
tryPerformCommands board unit (c : cs) = do
  board' <- tryPlaceUnit board unit
  tryPerformCommands board' (command unit c) cs

rotateUnitRight :: Unit -> Unit
rotateUnitRight unit =
    Unit {
      pivot = pivot unit,
      member = map rotatePositionRight $ member unit}

rotateUnitLeft :: Unit -> Unit
rotateUnitLeft unit = Unit { pivot = pivot unit, member = map rotatePositionLeft $ member unit }

canPlaceUnit :: Board -> Unit -> Bool
canPlaceUnit board unit =
    let cells = getCellsOnBoard unit
    in canPlaceCells board cells

tryPlaceUnit :: Board -> Unit -> Maybe Board
tryPlaceUnit board unit =
    let cells = getCellsOnBoard unit
        members = foldl fillCell board cells
    in if canPlaceCells board cells
       then Just members
       else Nothing

rotatePositionRight :: Position -> Position
rotatePositionRight p = foldl move (0, 0) $ map rotateDirectionRight $ getPathToPivot p

rotatePositionLeft :: Position -> Position
rotatePositionLeft p = foldl move (0, 0) $ map rotateDirectionLeft $ getPathToPivot p

rotateDirectionRight :: Direction -> Direction
rotateDirectionRight direction =
    case direction of
      East -> NorthWest
      SouthEast -> NorthEast
      SouthWest -> East
      West -> SouthEast
      NorthWest -> SouthWest
      NorthEast -> West

rotateDirectionLeft :: Direction -> Direction
rotateDirectionLeft direction =
    case direction of
      East -> SouthWest
      SouthEast -> West
      SouthWest -> NorthWest
      West -> NorthEast
      NorthWest -> East
      NorthEast -> SouthEast

getPathToPivot :: Position -> [Direction]
getPathToPivot = getPathToPivotHelper []

getPathToPivotHelper :: [Direction] -> Position -> [Direction]
getPathToPivotHelper directions p =
    if p == (0, 0) then directions
    else let direction = getDirection p
         in getPathToPivotHelper (direction : directions) $ move p direction

getDirection :: Position -> Direction
getDirection (x, y) =
    case (compare y 0, x < 0) of
      (EQ, True)  -> East
      (EQ, False) -> West
      (LT, True)  -> SouthEast
      (LT, False) -> SouthWest
      (GT, True)  -> NorthEast
      (GT, False) -> NorthWest

move :: Position -> Direction -> Position
move p direction =
    case direction of
      East -> moveEast p
      West -> moveWest p
      SouthEast -> moveSouthEast p
      SouthWest -> moveSouthWest p
      NorthEast -> moveNorthEast p
      NorthWest -> moveNorthWest p

command :: Unit -> Command -> Unit
command unit direction =
    let moveTo = fromPosition . move (toPosition $ pivot unit)
    in case direction of
         M E -> unit { pivot = moveTo East }
         M W -> unit { pivot = moveTo West }
         M SE -> unit { pivot = moveTo SouthEast }
         M SW -> unit { pivot = moveTo SouthWest }
         T CounterClockwise -> rotateUnitLeft unit
         T Clockwise -> rotateUnitRight unit

moveEast :: Position -> Position
moveEast (x, y) = (x + 1, y)

moveSouthEast :: Position -> Position
moveSouthEast (x, y) =
    if even y
    then (x, y + 1)
    else (x + 1, y + 1)

moveSouthWest :: Position -> Position
moveSouthWest (x, y) =
    if even y
    then (x - 1, y + 1)
    else (x, y + 1)

moveWest :: Position -> Position
moveWest (x, y) = (x - 1, y)

moveNorthWest :: Position -> Position
moveNorthWest (x, y) =
    if even y
    then (x - 1, y - 1)
    else (x, y - 1)

moveNorthEast :: Position -> Position
moveNorthEast (x, y) =
    if even y
    then (x, y - 1)
    else (x + 1, y - 1)


