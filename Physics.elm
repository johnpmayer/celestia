
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

module Physics where

import open Public.Vec2.Vec2
import open Public.TagTree.TagTree
import open Types
import open Utils

{- Mass -}

sumMasses : [PointMass] -> Float
sumMasses = sum . map (\pm -> pm.m)

massContrib : PointMass -> Vec2
massContrib pm = scaleVec pm.m <| extractVec pm

partMasses : Part -> [PointMass]
partMasses part = 
  let m = case part of
    (Brain size)    -> 3 * size.r * size.r
    (FuelTank size) -> 1.5 * size.l * size.w
    (Engine size)   -> 2 * size.r * size.r
  in [{ x=0, y=0, m=m }]

beamMasses : Beam -> [[PointMass]] -> [PointMass]
beamMasses = cnst concat 

attachMasses : Attach -> [PointMass] -> [PointMass]
attachMasses attach = map (translateAttach attach)

structureMasses : Structure -> [PointMass]
structureMasses = foldTagTree partMasses beamMasses attachMasses

totalMass : Structure -> Float
totalMass = sumMasses . structureMasses

centerOfMass : Structure -> Vec2
centerOfMass structure = 
  scaleVec (1 / totalMass structure) .
  sumVec .
  map massContrib .
  structureMasses <|
  structure

rotInertiaContrib : PointMass -> Float
rotInertiaContrib pm =
  let r = magnitude pm
  in pm.m * r * r

rotInertia : Structure -> Float
rotInertia structure = 
  sum .
  map rotInertiaContrib .
  map (subVec <| centerOfMass structure) .
  structureMasses <|
  structure

{- Thrust -}

partThrusts : [EngineConfig] -> Part -> [Thrust]
partThrusts ecs part = 
  case part of
    (Engine engine) -> 
      if any (\ec -> ec == engine.config) ecs
      then let force = -8 * engine.r
           in [ { disp = origin, force = { x = force, y = 0 } } ]
      else [] 
    _ -> []

beamThrusts : Beam -> [[Thrust]] -> [Thrust]
beamThrusts = cnst concat

attachThrusts : Attach -> [Thrust] -> [Thrust]
attachThrusts attach ts = 
  let offsetDisp t = { t | disp <- addVec { x = attach.offset, y = 0 } t.disp }
      rotateDisp t = { t | disp <- rotVec attach.theta t.disp }
      rotateForce t = { t | force <- rotVec attach.theta t.force }
  in map (offsetDisp . rotateDisp . rotateForce) <| ts

structureThrusts : [EngineConfig] -> Structure -> [Thrust]
structureThrusts ec = foldTagTree (partThrusts ec) beamThrusts attachThrusts

netForce : [EngineConfig] -> Structure -> Vec2
netForce ec s = sumVec . map .force <| structureThrusts ec s

netAcceleration : [EngineConfig] -> Structure -> Vec2
netAcceleration ec s = scaleVec (1 / totalMass s) <| netForce ec s

torqueMag : Thrust -> Float
torqueMag t = crossVecMag t.disp t.force

netTorque : [EngineConfig] -> Structure -> Float
netTorque ec s =
  let com = centerOfMass s
      thrusts = structureThrusts ec s
      center thrust = { thrust | disp <- subVec com thrust.disp }
      torqueForces = map center thrusts
  in sum . map torqueMag <| torqueForces 

netRotAcceleration : [EngineConfig] -> Structure -> Float
netRotAcceleration ec s = netTorque ec s / rotInertia s

netDelta : [EngineConfig] -> Structure -> MotionDelta
netDelta ec s = { a = netAcceleration ec s, alpha = netRotAcceleration ec s }

applyBrakes : MotionState -> { newOmega : Float, newV : { x : Float, y : Float } }
applyBrakes state = 
  let vx = state.v.x
      vy = state.v.y
      w = state.omega
      posfactor = 0.01
      rotfactor = 0.0005
      damp factor n =  
        if n < factor && n > -1 * factor 
        then 0 
        else 
          if n > 0
          then n - factor
          else n + factor
  in { newOmega = damp rotfactor w
     ,  newV = { x = damp posfactor vx, y = damp posfactor vy } }

updateMotion : (Bool,MotionDelta) -> MotionState -> MotionState
updateMotion (brakes,delta) state = 
  if brakes
  then 
    let fakeDelta = applyBrakes state
        newOmega = fakeDelta.newOmega
        midOmega = (state.omega + newOmega) / 2
        newTheta = state.pos.theta + midOmega
        midTheta = (state.pos.theta + newTheta) / 2
        newV = fakeDelta.newV
        midV = midVec state.v newV
        statePos = state.pos
        newPos = addVec midV <| { statePos | theta <- newTheta }
    in { state | pos <- newPos, v <- newV, omega <- newOmega }
  else
    let newOmega = delta.alpha + state.omega
        midOmega = (state.omega + newOmega) / 2
        newTheta = state.pos.theta + midOmega
        midTheta = (state.pos.theta + newTheta) / 2
        absA = rotVec midTheta delta.a
        newV = addVec absA state.v
        midV = midVec state.v newV
        statePos = state.pos
        newPos = addVec midV <| { statePos | theta <- newTheta }
    in { state | pos <- newPos, v <- newV, omega <- newOmega }
