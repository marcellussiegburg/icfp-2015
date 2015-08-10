module Solver where

import Data
import Commands

topDownRun :: Board -> [Unit] -> [Command]
topDownRun board = topDownRunHelper (board, [])

topDownRunHelper :: (Board, [Command]) -> [Unit] -> [Command]
topDownRunHelper (board, commands) units =
    case units of
      [] -> commands
      u:us -> if canPlaceUnit board u
              then let (b, cs) = topDown board u
                   in topDownRunHelper (b, commands ++ cs) us
              else commands

topDown :: Board -> Unit -> (Board, [Command])
topDown board = topDownHelper (board, [])

topDownHelper :: (Board, [Command]) -> Unit -> (Board, [Command])
topDownHelper (board, commands) unit =
    case tryPlaceUnit board unit of
      Nothing ->
          case commands of
             [] -> (board, [])
             _ -> (board, reverse $ tail commands)
      Just board' ->
          let dir = if even (y $ pivot unit) then M SE else M SW
          in topDownHelper (board', dir : commands) $ command unit dir


