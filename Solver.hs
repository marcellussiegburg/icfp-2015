module Solver where

import Data
import Commands

topDownPhraseRun :: Board -> [Unit] -> [[Command]] -> [Command]
topDownPhraseRun = topDownPhraseRun'

topDownPhraseRun' :: Board -> [Unit] -> [[Command]] -> [Command]
topDownPhraseRun' board units [] = topDownRun board units
topDownPhraseRun' board (u:us) commands =
    if canPlaceUnit board u
    then case topDownPhraseRunHelper board u commands of
           ([], _) -> let mbc = topDownOnce board u
                         in case mbc of
                              Nothing -> [if even (y $ pivot u) then M SE else M SW]
                              Just (board', tdc) -> tdc : topDownPhraseRun' board' (command u tdc : us) commands
           (cmds, b) -> cmds ++ topDownPhraseRun' board (foldl command u cmds : us) commands
    else []

topDownPhraseRunHelper :: Board -> Unit -> [[Command]] -> ([Command], Board)
topDownPhraseRunHelper board unit [] = ([], board)
topDownPhraseRunHelper board unit (cs:css) =
    let board' = tryPerformCommands board unit cs
    in case board' of
         Nothing -> topDownPhraseRunHelper board unit css
         Just b -> (cs, b)

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
             _ -> (board, reverse commands)
      Just board' ->
          let dir = if even (y $ pivot unit) then M SE else M SW
          in topDownHelper (board', dir : commands) $ command unit dir

topDownOnce :: Board -> Unit -> Maybe (Board, Command)
topDownOnce board unit =
    let dir = if even (y $ pivot unit) then M SE else M SW
    in case tryPlaceUnit board (command unit dir) of
         Nothing -> Nothing
         Just board' -> Just (board', dir)

