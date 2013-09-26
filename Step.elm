
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


module Step where

import Char as C
import Dict as D
import open Either

import Public.State.State as ST
import Public.State.State (State)

import Public.Vec2.Vec2 as V

import open Types
import open Utils
import open Physics
import open ShipWright

gamePaused : State GameState Bool
gamePaused =
  let (>>=) = ST.bindS
      pure = ST.returnS
  in 
    ST.get >>= (\state ->
    pure state.mode.pause)

noStep : GameStep
noStep = ST.returnS ()

step : GameInput -> GameStep
step input = 
  let (>>=) = ST.bindS
      pure = ST.returnS
  in case input.trigger of
    Modal m -> updateMode m
    Click -> build
    Pointer pointer -> updatePhantom pointer
    FPS t ->
      gamePaused >>= (\paused ->
      if paused
      then noStep
      else
        (focusControls input) >>= (\_ ->
        physicsStep))

updateMode : Modal -> GameStep
updateMode m =
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    case m of
      Cycle -> rotateFocus
      Pause -> 
        ST.get >>= (\state ->
        let mode = state.mode
            pause = not state.mode.pause
        in ST.put { state | mode <- { mode | pause <- pause } })
      Exit -> quitBuild
      Number n -> updateModeTree n

updateModeTree : Int -> GameStep
updateModeTree n =
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    let mode = state.mode
        buildMode = mode.build
        part = case n of
          1 -> Just <| Brain { r=10 }
          2 -> Just <| FuelTank { l=20, w=10 }
          3 -> Just <| Engine { r=10, config=Forward }
          _ -> Nothing
        newBuildMode = { buildMode | part <- part }
        newMode = { mode | build <- newBuildMode }
        newState = { state | mode <- newMode }
    in ST.put newState)

quitBuild : GameStep
quitBuild = 
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    let mode = state.mode
        buildMode = mode.build
        newBuildMode = BuildMode buildMode.entity Place Nothing Nothing Nothing
        newMode = { mode | build <- newBuildMode }
    in ST.put { state | mode <- newMode })

rotateFocus : GameStep
rotateFocus =
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    let focus = state.focus
        newFocus = (focus + 1) `mod` 5
        newState = { state | focus <- newFocus }
    in ST.put newState)

setStage : BuildStage -> GameStep
setStage newStage = ST.updateS <| (\state ->
  let mode = state.mode
      buildMode = mode.build
      newBuildMode = { buildMode | stage <- newStage }
      newMode = { mode | build <- newBuildMode }
  in { state | mode <- newMode })

build : GameStep
build = 
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    let buildMode = state.mode.build
        stage = buildMode.stage
    in case buildMode.part of
      Nothing -> noStep
      Just _ -> case stage of
          Place -> 
            case fixPhantom state of
              Nothing -> noStep
              Just cache ->
                setStage <| Rotate cache
          Rotate cache -> 
            ST.updateS addPhantom >>= (\_ ->
            setStage Place))

updatePhantom : (Int,Int) -> GameStep
updatePhantom pointer = 
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    case state.mode.build.part of
      Nothing -> noStep
      Just _ ->
        let relPointerV = V.fromIntPair pointer
            myShip = D.lookup state.focus state.entities
            buildShip = D.lookup state.mode.build.entity state.entities
        in case (myShip, buildShip) of
          (Just myShip, Just buildShip) ->
            let absPointerV = V.addVec state.cache.camera relPointerV
                buildPos = buildShip.motion.pos
                localPointerV = V.rotVec (-buildPos.theta) <| V.subVec buildPos absPointerV
                mode = state.mode
                buildMode = mode.build
            in case buildMode.stage of
              Place ->
                let best = Just <| bestPlacement localPointerV buildShip
                    newBuildMode = { buildMode | placement <- best }
                    newMode = { mode | build <- newBuildMode }
                    newState = { state | mode <- newMode }
                in ST.put newState
              Rotate { localDisp, relOrientation } -> 
                let dispPointer = V.subVec localDisp localPointerV
                    absRotate = atan2 dispPointer.y dispPointer.x
                    localRotate = absRotate - relOrientation 
                    newBuildMode = { buildMode | absRotate <- Just localRotate }
                    newMode = { mode | build <- newBuildMode }
                    newState = { state | mode <- newMode }
                in ST.put newState
          _ -> noStep)

focusControls : GameInput -> GameStep
focusControls input = 
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    let focus = state.focus
        entities = state.entities
        myShip = D.lookup focus entities
    in case myShip of
      Nothing -> noStep
      Just e ->
        let newMyShip = { e | controls <- input.engines }
            newEntities = D.insert focus newMyShip entities
            newState = { state | entities <- newEntities }
        in ST.put newState)

entityPureStep : Entity -> Entity
entityPureStep e =
  let newMotion = case e.controls of
        Left _ -> updateBrakes e.cache e.motion
        Right engines -> 
          let delta = netDelta engines e.cache
          in updateMotion e.cache delta e.motion
  in { e | motion <- newMotion }

physicsStep : GameStep
physicsStep = 
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    let newEntities = D.map entityPureStep state.entities
        focus = state.focus
        newCache = genGameStateCache focus newEntities
        newState = { state | entities <- newEntities, cache <- newCache }
    in ST.put newState)
