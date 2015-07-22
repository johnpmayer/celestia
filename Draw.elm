
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

module Draw where

import Color exposing (..)
import Graphics.Collage exposing (..)
import List exposing (any)
import Time exposing (Time)
import Transform2D exposing (Transform2D)
import Transform2D as T

import Data.TagTree exposing (..)
import Data.Vec2 exposing (..)

import Physics exposing (..)
import Types exposing (..)

translation : Float -> Float -> Transform2D
translation = T.matrix 1 0 0 1

drawPart : Time -> List EngineConfig -> Part -> Form
drawPart noise ecs part =
  let hashedNoise = (truncate noise) % 150
      gasColor = rgb 255 (255 - hashedNoise) 0
  in case part of
    (Brain size) -> 
      filled blue <| circle size.r 
    (FuelTank size) -> 
      filled green <| rect size.l size.w 
    (Engine engine) -> 
      group <|
        (rotate pi << filled red <| ngon 3 engine.r) ::
        (if any (\ec -> ec == engine.config) ecs
        then [ move ((0.4 * engine.r), 0) << rotate pi << filled gasColor <| ngon 3 (engine.r * 0.8) ]
        else [])

drawAttach : Attach -> Form -> Form
drawAttach {offset,theta} subForm =
  let modelM = T.multiply (translation offset 0) (T.rotation theta)
  in groupTransform modelM [subForm]

drawBeam : Beam -> List Form -> Form
drawBeam beam subForms =
  let l = beam.r
      w = 2
      beamForm = move (l * 0.5, 0) << filled white <| rect l w
  in group (beamForm :: subForms)

drawStructure : Time -> List EngineConfig -> Structure -> Form
drawStructure noise ec structure =
  foldTagTree (drawPart noise ec) drawBeam drawAttach structure

drawEntity : Time -> Entity -> Form
drawEntity noise { controls, motion, cache } = 
  let rotM = T.rotation motion.pos.theta
      moveM = translation motion.pos.x motion.pos.y
      modelM = T.multiply moveM rotM
      engines = case controls of
        Brakes -> []
        Active ecs -> ecs
  in groupTransform modelM <| [ drawStructure noise engines cache.structure ]

drawBuildArea : BuildMode -> Form
drawBuildArea mode = case mode.placement of
  Nothing -> group []
  _       -> filled (rgba 255 255 0 0.2) <| oval 200 200
