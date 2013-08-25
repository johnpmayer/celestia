
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


module Demo where

import Dict (Dict)
import Dict as D

import Draw (drawEntity)
import Public.State.State (execState)
import Step (GameState, Mode, None, step)
import GameInputs (gameInputs)
import open Types
import open Utils
import open Public.Vec2.Vec2

import Main (simpleShip)

{- Setup initial game state and initialize the loop -}

initialShips : [Entity]
initialShips = 
  [ { controls = [], motion = { pos = { x =   0, y =   0, theta = 0 }, v = { x = 0, y = 0 }, omega = 0 }, structure = simpleShip }
  , { controls = [], motion = { pos = { x =  60, y =  50, theta = 1 }, v = { x = 0, y = 0 }, omega = 0 }, structure = simpleShip }
  , { controls = [], motion = { pos = { x = -80, y =  40, theta = 4 }, v = { x = 0, y = 0 }, omega = 0 }, structure = simpleShip }
  , { controls = [], motion = { pos = { x = 120, y = -20, theta = 3 }, v = { x = 0, y = 0 }, omega = 0 }, structure = simpleShip }
  ]

initialEntities : Dict Int Entity
initialEntities = 
  D.fromList .
  zip [0,1,2,3] <|
  initialShips

initialState : GameState
initialState = { entities = initialEntities, mode = initialMode, focus = 0 }

initialMode : Mode
initialMode = { pause = False, build = None }

current : Signal GameState
current = foldp (execState . step) initialState gameInputs

{- Render the display -}

draw : GameState -> [Form]
draw gs = 
  let entities = D.values <| gs.entities
      camera = case D.lookup gs.focus gs.entities of
        Nothing -> origin
        Just e -> extractVec e.motion.pos
      cameraTransform = vecTranslate <| negVec camera
  in [groupTransform cameraTransform <| map (drawEntity 0) entities]

main = combineSElems <|
  [ spaceBlack (800,600) . draw <~ current
  , asText <~ current
  , asText <~ gameInputs
  ]
