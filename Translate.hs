{-# LANGUAGE OverloadedStrings #-}
module Translate where

import Prelude hiding (foldr, head)
import Data.List (isPrefixOf)
import Data.Text.Lazy (Text, append, cons, find, foldr, head)
import Data.Map.Lazy (Map, toAscList)

import Data

west = "p'!.03"
east = "bcefy2"
southwest = "aghij4"
southeast = "lmno 5"
clockwise = "dqrvz1"
counterClockwise = "kstuwx"

transformWithPhrases :: Map Text [Command] -> [Command] -> Text
transformWithPhrases map = transformWithPhrases' (toAscList map) (toAscList map)

transformWithPhrases' :: [(Text, [Command])] -> [(Text, [Command])] -> [Command] -> Text
transformWithPhrases' phrases all [] = ""
transformWithPhrases' [] all (c:cs) = append (transform [c]) $ transformWithPhrases' all all cs
transformWithPhrases' (phrase : phrases) all commands =
    if (snd phrase) `isPrefixOf` commands
    then append (fst phrase) $ transformWithPhrases' all all $ drop (length $ snd phrase) commands
    else transformWithPhrases' phrases all commands

transform :: [Command] -> Text
transform [] = ""
transform (command:commands) =
    cons (case command of
            M E -> head east
            M W -> head west
            M SE -> head southeast
            M SW -> head southwest
            T Clockwise -> head clockwise
            T CounterClockwise -> head counterClockwise
         ) $ transform commands
              
getPhraseCommands :: Text -> Maybe [Command]
getPhraseCommands phrase = fmap reverse $
    foldr (\l mcs -> case getPhraseCommand l of
                       Nothing -> Nothing
                       Just c -> fmap (c :) mcs) (Just []) phrase

getPhraseCommand :: Char -> Maybe Command
getPhraseCommand letter =
    let e = find (== letter) east
        w = find (== letter) west
        se = find (== letter) southeast
        sw = find (== letter) southwest
        cw = find (== letter) clockwise
        ccw = find (== letter) counterClockwise
    in case (e, w, se, sw, cw, ccw) of
         (Just _, _, _, _, _, _) -> Just $ M E
         (_, Just _, _, _, _, _) -> Just $ M W
         (_, _, Just _, _, _, _) -> Just $ M SE
         (_, _, _, Just _, _, _) -> Just $ M SW
         (_, _, _, _, Just _, _) -> Just $ T Clockwise
         (_, _, _, _, _, Just _) -> Just $ T CounterClockwise
         (_, _, _, _, _, _) -> Nothing


