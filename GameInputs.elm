
{-
    Copyright (c) John P Mayer, Jr 2013

    This file is part of celestia.

    celestia is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    celestia is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with celestia.  If not, see <http://www.gnu.org/licenses/>.
-}


module GameInputs where

import Keyboard as K
import Mouse as M
import Time as T

import open Types 
import open Utils

gameInputs : Signal GameInput
-- TODO gameInputs = GameInput
gameInputs = sampleOn triggers <| (\engines pointer trigger -> { engines=engines, pointer=pointer,trigger=trigger}) 
          <~ (controlEngines <~ K.wasd) 
          ~ ((convertPos (800,600)) <~ M.position)
          ~ triggers

triggers : Signal Trigger
triggers = merges [ clicks, modes, ticks ]

clicks : Signal Trigger
clicks = (cnst Click) <~ M.clicks

modes : Signal Trigger
modes = constant <| Modal Exit

ticks : Signal Trigger
ticks = FPS <~ (T.fps 25)
