
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

import Dict as D
import open Either

import Char as C

import open Types

import Public.State.State as ST

import open Utils

import open Physics

step : GameInput -> GameStep
step input = 
  let (>>=) = ST.bindS
      pure = ST.returnS
  in case input.trigger of
    Modal m -> updateMode m
    Click -> rotateFocus
    FPS t ->
      (focusControls input) >>= (\_ ->
      physicsStep)

updateMode : Modal -> GameStep
updateMode m =
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    let mode = state.mode
        pause = case m of
          Number n -> 
            if n == C.toCode 'p'
            then not state.mode.pause
            else state.mode.pause
          _ -> state.mode.pause
    in ST.put { state | mode <- { mode | pause <- pause } })

rotateFocus : GameStep
rotateFocus =
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    let focus = state.focus
        newFocus = (focus + 1) `mod` 4
        newState = { state | focus <- newFocus }
    in ST.put newState)

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
      Nothing -> pure ()
      Just e ->
        let newMyShip = { e | controls <- input.engines }
            newEntities = D.insert focus newMyShip entities
            newState = { state | entities <- newEntities }
        in ST.put newState)

entityPureStep : Entity -> Entity
entityPureStep e =
  let rest = {a={x=0,y=0},alpha=0}
      newMotion = case e.controls of
        Left _ -> updateMotion (True,rest) e.motion
        Right engines -> 
          let delta = netDelta engines e.structure
          in updateMotion (False,delta) e.motion
  in { e | motion <- newMotion }

physicsStep : GameStep
physicsStep = 
  let (>>=) = ST.bindS
      pure = ST.returnS
  in
    ST.get >>= (\state ->
    let newEntities = D.map entityPureStep state.entities
        newState = { state | entities <- newEntities }
    in ST.put newState)
