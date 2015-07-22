
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

module Types where

import Dict exposing (Dict)
import Time exposing (Time)

import Control.State exposing (..)
import Data.TagTree exposing (..)
import Data.Vec2 exposing (..)

{- Structures -}

type alias Attach = { offset : Float, theta : Float }

translateAttach : Attach -> Vec2Ext a -> Vec2Ext a
translateAttach {offset,theta} = addVec {x=offset,y=0} << rotVec theta

type alias Beam = { r : Float }

type alias StructureExt a = TagTree Part a Attach
type alias Structure = StructureExt Beam

beam : Beam -> List (Attach, Structure) -> Structure
beam = Node

part : Part -> Structure
part = Leaf

type EngineConfig = Disabled
                  | Forward 
                  | Reverse
                  | TurnLeft 
                  | TurnRight

nextConfig : EngineConfig -> EngineConfig
nextConfig e = case e of
  Disabled  -> Forward
  Forward   -> Reverse
  Reverse   -> TurnLeft
  TurnLeft  -> TurnRight
  TurnRight -> Disabled

type Part = Brain { r : Float }
          | FuelTank { l : Float, w : Float }
          | Engine { r : Float, config : EngineConfig }

{- Labeling -}

type alias LabelBeamExt a = { a | id : Int }
type alias LabelBeam = LabelBeamExt Beam

type alias LabelStructure = TagTree Part LabelBeam Attach

fresh : State Int Int
fresh =
  bindS get (\i ->
  bindS (put (i + 1)) (\_ ->
  returnS i))

labelBeams : Structure -> LabelStructure
labelBeams s =
  let labelBeam beam = fmapS (\i -> { beam | id = i }) fresh
      modifyStructure = walkModify returnS labelBeam returnS
  in evalState (modifyStructure s) 0

{- Physics -}

type alias PointMass = Vec2Ext { m : Float }

type Moment = Point PointMass 
            | ParallelAxis (Vec2Ext { m : Float, localMoment : Float })

type alias Thrust = { disp : Vec2, force : Vec2 }

type alias Position = Vec2Ext { theta : Float }

type alias MotionState = { pos : Position, v : Vec2, omega : Float }

type alias MotionDelta = { a : Vec2, alpha : Float }

type alias Entity = { controls : Controls, motion : MotionState, cache : EntityCache }

type alias EntityCache = { structure : Structure, comOffset : Vec2, totalMass : Float, rotInertia : Float }

{- GameInputs -}

type alias GameInput = { engines : Controls, window : (Int,Int), trigger : Trigger }

type Controls = Brakes | Active (List EngineConfig)

type Trigger 
  = Click
  | Modal Modal
  | FPS Time
  | Pointer (Int,Int)

type Modal
  = Pause
  | Cycle
  | Exit
  | Number Int

type alias GameState = { entities : Dict Int Entity, mode : Mode, focus : Int, cache : GameStateCache }

type alias GameStateCache = { camera : Vec2 }

type alias Mode = { pause : Bool, build : BuildMode }

type alias LabelDist = { id:Int, r:Float, offset:Float }

type alias BuildPoint = {id : Int, start : Vec2, end : Vec2}

type alias BuildCache = { localDisp : Vec2, relOrientation : Float }

type BuildStage
  = Place
  | Rotate BuildCache

type alias BuildMode = { entity : Int, stage : BuildStage, absRotate : Maybe Float, placement : Maybe LabelDist, part : Maybe Structure }

type alias GameStep = State GameState ()

