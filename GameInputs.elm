
module GameInputs where

import Keyboard as K
import Mouse as M
import Time as T

import open Types 
import Utils (cnst, controlEngines)

gameInputs : Signal GameInput
gameInputs = GameInput 
          <~ (controlEngines <~ K.wasd) 
          ~ M.position 
          ~ triggers

triggers : Signal Trigger
triggers = merges [ clicks, modes, ticks ]

clicks : Signal Trigger
clicks = (cnst Click) <~ M.clicks

modes : Signal Trigger
modes = constant <| Modal Exit

ticks : Signal Trigger
ticks = FPS <~ (T.fps 25)
