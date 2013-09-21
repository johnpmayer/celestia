
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

import open Either

import Char as C
import Dict as D
import Dict (Dict)
import Keyboard as K
import Keyboard (KeyCode)
import Mouse as M
import Time as T
import Window as W

import open Types 
import open Utils

gameInputs : Signal GameInput
-- TODO gameInputs = GameInput
gameInputs = sampleOn triggers <| (\engines pointer window trigger -> { engines=engines, pointer=pointer, window=window, trigger=trigger}) 
          <~ engines
          ~ pointer
          ~ W.dimensions
          ~ triggers

engines : Signal (Either Brakes [EngineConfig])
engines =
  let brakes = K.isDown . C.toCode <| 'x'
      controls = controlEngines <~ K.wasd
      override b ecs = if b then Left Brakes else Right ecs
  in override <~ brakes ~ controls

pointer : Signal (Int,Int)
pointer = convertPos <~ W.dimensions ~ M.position

triggers : Signal Trigger
triggers = merges [ clicks, modes, ticks ]

clicks : Signal Trigger
clicks = (cnst Click) <~ M.clicks

watchKeymap : Dict KeyCode Modal
watchKeymap = D.fromList
  [ (C.toCode '1', Number 1)
  , (C.toCode '2', Number 2)
  , (C.toCode '3', Number 3)
  , (C.toCode '4', Number 4)
  , (C.toCode '5', Number 5)
  , (C.toCode 'p', Pause)
  , (C.toCode 'c', Cycle)
  , (27, Exit)
  ]

modeKeysDown : Signal Modal
modeKeysDown =
  let watchKeys = D.toList watchKeymap
      keysDown = map (\(kc,modal) -> lift (cnst modal) . keepIf id True <| K.isDown kc) watchKeys
  in merges keysDown

modes : Signal Trigger
modes = Modal <~ modeKeysDown

ticks : Signal Trigger
ticks = FPS <~ (T.fps 25)