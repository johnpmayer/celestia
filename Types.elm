
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

import Dict (Dict)
import Either (Either)

import open Public.State.State
import open Public.TagTree.TagTree
import open Public.Vec2.Vec2

type Position = { x : Float, y : Float, theta : Float }

{- Structures -}

type Attach = { offset : Float, theta : Float }

translateAttach : Attach -> Vec2Ext a -> Vec2Ext a
translateAttach attach vext = 
  let dX = attach.offset * cos attach.theta
      dY = attach.offset * sin attach.theta
  in addVec { x = dX, y = dY } vext

type Beam = { r : Float }

type StructureExt a = TagTree Part a Attach
type Structure = StructureExt Beam

beam : Beam -> [(Attach, Structure)] -> Structure
beam = Node

part : Part -> Structure
part = Leaf

data EngineConfig = Disabled
                  | Forward 
                  | Reverse
                  | TurnLeft 
                  | TurnRight

data Part = Brain { r : Float }
          | FuelTank { l : Float, w : Float }
          | Engine { r : Float, config : EngineConfig }

{- Labeling -}

type LabelBeamExt a = { a | id : Int }
type LabelBeam = LabelBeamExt Beam

type LabelStructure = TagTree Part LabelBeam Attach

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

type PointMass = Vec2Ext { m : Float }

data Moment = Point PointMass 
            | ParallelAxis (Vec2Ext { m : Float, localMoment : Float })

type Thrust = { disp : Vec2, force : Vec2 }

type MotionState = { pos : Position, v : Vec2, omega : Float }

type MotionDelta = { a : Vec2, alpha : Float }

type Entity = { controls : Either Brakes [EngineConfig], motion : MotionState, structure : Structure }

{- GameInputs -}

type GameInput = { engines : Either Brakes [EngineConfig], pointer : (Int,Int), window : (Int,Int), trigger : Trigger }

data Brakes = Brakes

data Trigger 
  = Click
  | Modal Modal
  | FPS Time

data Modal
  = Pause
  | Cycle
  | Exit
  | Number Int

type GameState = { entities : Dict Int Entity, mode : Mode, focus : Int }

type Mode = { pause : Bool, build : BuildMode }

type LabelDist = { id:Int, r:Float, offset:Float }

type BuildMode = { entity : Int, placement : Maybe LabelDist, part : Part }

type GameStep = State GameState ()

