
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

import Dict (empty, values)

import Draw (drawEntity)
import Public.State.State (execState)
import Step (GameState, Mode, None, step)
import GameInputs (gameInputs)
import open Utils

{- Setup initial game state and initialize the loop -}

initialState : GameState
initialState = { entities = empty, mode = initialMode }

initialMode : Mode
initialMode = { pause = False, build = None }

current : Signal GameState
current = foldp (execState . step) initialState gameInputs

{- Render the display -}

draw : GameState -> [Form]
draw gs = 
  let entities = map (drawEntity 0) . values . .entities <| gs
  in entities

main = combineSElems <|
  [ asText <~ current
  , spaceBlack (400,400) . draw <~ current
  ]
