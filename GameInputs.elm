
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

import Char as C
import Dict as D
import Dict exposing (Dict)
import Keyboard as K
import Keyboard exposing (KeyCode)
import Mouse as M
import Signal exposing (..)
import Time as T
import Window as W

import Types  exposing (..)
import Utils exposing (..)

gameInputs : Signal GameInput
-- TODO gameInputs = GameInput
gameInputs = sampleOn triggers <| (\engines window trigger -> { engines=engines, window=window, trigger=trigger}) 
          <~ engines
          ~ dimensions
          ~ triggers

dimensions : Signal (Int, Int)
dimensions = W.dimensions

engines : Signal Controls
engines =
  let brakes = K.isDown << C.toCode <| 'X'
      controls = controlEngines <~ K.wasd
      override b ecs = if b then Brakes else Active ecs
  in override <~ brakes ~ controls

triggers : Signal Trigger
triggers = mergeMany [ clicks, modes, ticks, pointer ]

clicks : Signal Trigger
clicks = (cnst Click) <~ M.clicks

watchKeymap : Dict KeyCode Modal
watchKeymap = D.fromList
  [ (C.toCode '1', Number 1)
  , (C.toCode '2', Number 2)
  , (C.toCode '3', Number 3)
  , (C.toCode '4', Number 4)
  , (C.toCode '5', Number 5)
  , (C.toCode 'P', Pause)
  , (C.toCode 'C', Cycle)
  , (27, Exit)
  ]

modeKeysDown : Signal Modal
modeKeysDown =
  let watchKeys = D.toList watchKeymap
      keysDown : List (Signal Modal) 
      keysDown = List.map (\(kc,modal) -> Signal.map (cnst modal) << filter identity True <| K.isDown kc) watchKeys
  in mergeMany keysDown

modes : Signal Trigger
modes = Modal <~ modeKeysDown

ticks : Signal Trigger
ticks = FPS <~ (T.fps 25)

pointer : Signal Trigger
pointer = Pointer <~ (convertPos <~ dimensions ~ M.position)

