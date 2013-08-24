
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

import Main (simpleShip)

{- Setup initial game state and initialize the loop -}

initialShip : Entity
initialShip = { controls=[], motion={ pos = { x = 0, y = 0, theta = 0 }, v = { x = 0, y = 0 }, omega = 0 }, structure=simpleShip }

initialEntities : Dict Int Entity
initialEntities = D.singleton 0 initialShip

initialState : GameState
initialState = { entities = initialEntities, mode = initialMode }

initialMode : Mode
initialMode = { pause = False, build = None }

current : Signal GameState
current = foldp (execState . step) initialState gameInputs

{- Render the display -}

draw : GameState -> [Form]
draw gs = 
  let entities = D.values <| gs.entities
  in map (drawEntity 0) entities

main = combineSElems <|
  [ spaceBlack (800,600) . draw <~ current
  , asText <~ current
  , asText <~ gameInputs
  ]
