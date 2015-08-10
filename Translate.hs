{-# LANGUAGE OverloadedStrings #-}
module Translate where

import Prelude hiding (head)
import Data.Text.Lazy

import Data

west = "p'!.03"
east = "bcefy2"
southwest = "aghij4"
southeast = "lmno 5"
clockwise = "dqrvz1"
counterClockwise = "kstuwx"

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
              
