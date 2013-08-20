
module Trigger where

import Mouse as M
import Time as T

import Utils (cnst)

data Trigger 
  = Click
  | Position (Int,Int)
  | Modal Modal
  | FPS Time

triggers : Signal Trigger
triggers = merges [ clicks, positions, modes, ticks ]

clicks : Signal Trigger
clicks = (cnst Click) <~ M.clicks

positions : Signal Trigger
positions = Position <~ M.position

data Modal
  = Pause
  | Exit
  | Number Int

modes : Signal Trigger
modes = constant <| Modal Exit

ticks : Signal Trigger
ticks = FPS <~ (T.fps 25)
