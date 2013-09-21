
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

import open Either

import Dict (Dict)
import Dict as D

import open Physics

import Draw (drawEntity)
import Public.State.State (execState)
import Step (GameState, Mode, None, step)
import GameInputs (gameInputs, modes)
import open Types
import open Utils
import open Public.Vec2.Vec2

import Main (simpleShip)
import ShipWright (placePhantomPart)

{- Setup initial game state and initialize the loop -}

spine : Structure
spine = beam {r = 200} [] 

station : Entity
station = { controls = Right [], motion = { pos = { x = 0, y = 200, theta = 0 }, v = { x = 0, y = 0 }, omega = 0 }, cache = genCache spine }

initialShips : [Entity]
initialShips = 
  [ { controls = Right [], motion = { pos = { x =   0, y =   0, theta = 0 }, v = { x = 0, y = 0 }, omega = 0 }, cache = genCache simpleShip }
  , { controls = Right [], motion = { pos = { x =  60, y =  50, theta = 1 }, v = { x = 0, y = 0 }, omega = 0 }, cache = genCache simpleShip }
  , { controls = Right [], motion = { pos = { x = -80, y =  40, theta = 4 }, v = { x = 0, y = 0 }, omega = 0 }, cache = genCache simpleShip }
  , { controls = Right [], motion = { pos = { x = 120, y = -20, theta = 3 }, v = { x = 0, y = 0 }, omega = 0 }, cache = genCache simpleShip }
  ]

initialEntities : Dict Int Entity
initialEntities = 
  D.fromList <| (4, station) :: (zip [0,1,2,3] initialShips)

initialState : GameState
initialState = { entities = initialEntities, mode = initialMode, focus = 0 }

initialMode : Mode
initialMode = { pause = False, build = initialBuildMode }

initialPlacement : Maybe LabelDist
initialPlacement = Just { id=0, r=20, offset=30 }

initialBuildMode : BuildMode
initialBuildMode = { entity = 4, placement = initialPlacement, part = Engine { r = 10, config = Forward } }

current : Signal GameState
current = foldp (execState . step) initialState gameInputs

absPointer : Signal Vec2
absPointer = ((\(x,y) -> {x=toFloat x,y=toFloat y}) . .pointer) <~ gameInputs

withPhantom : Signal GameState
withPhantom = 
  let addPhantom state = 
    let buildMode = state.mode.build
        e = buildMode.entity
        placement = buildMode.placement
    in case placement of
      Nothing -> state
      Just best -> 
        let place = placePhantomPart buildMode.part best
            es = updateDict e place state.entities
        in { state | entities <- es }
  in addPhantom <~ current

{- Render the display -}

draw : Float -> GameState -> [Form]
draw n gs = 
  let entities = D.values <| gs.entities
      camera = case D.lookup gs.focus gs.entities of
        Nothing -> origin
        Just e -> 
          let rootPos = extractVec e.motion.pos
              comOffset = rotVec e.motion.pos.theta e.cache.comOffset
          in addVec comOffset rootPos 
      cameraTransform = vecTranslate <| negVec camera
      entityForms = map (drawEntity <| n) entities
  in [groupTransform cameraTransform <| entityForms]

main = combineSElems outward <|
  [ spaceBlack <~ (.window <~ gameInputs) ~ (draw <~ (fst <~ timestamp gameInputs) ~ withPhantom)
  , combineSElems down
    [ (color white . asText) <~ gameInputs
    , (color white . asText . .focus) <~ current
    , (color white . asText . .mode) <~ current
    , (color white . asText) <~ modes
    ]
  ]