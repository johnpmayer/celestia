
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

import Data.Vec2 exposing (..)
import Data.TagTree exposing (..)
import List exposing (any, concat, map, sum)
import Types exposing (..)
import Utils exposing (..)

{- Mass -}

sumMasses : List PointMass -> Float
sumMasses = sum << map (\pm -> pm.m)

massContrib : PointMass -> Vec2
massContrib pm = scaleVec pm.m <| extractVec pm

partMasses : Part -> List PointMass
partMasses part = 
  let m = case part of
    (Brain size)    -> 3 * size.r * size.r
    (FuelTank size) -> 1.5 * size.l * size.w
    (Engine size)   -> 2 * size.r * size.r
  in [{ x=0, y=0, m=m }]

beamMasses : Beam -> List (List PointMass) -> List PointMass
beamMasses beam subs = {x = beam.r / 2, y = 0, m = 0.1 * beam.r} :: concat subs 

attachMasses : Attach -> List PointMass -> List PointMass
attachMasses attach = map (translateAttach attach)

structureMasses : Structure -> List PointMass
structureMasses = foldTagTree partMasses beamMasses attachMasses

totalMass : Structure -> Float
totalMass = sumMasses << structureMasses

centerOfMass : Structure -> Vec2
centerOfMass structure = 
  scaleVec (1 / totalMass structure) <<
  sumVec <<
  map massContrib <<
  structureMasses <|
  structure

partMoment : Part -> List Moment
partMoment = map Point << partMasses

beamMoment : Beam -> List (List Moment) -> List Moment
beamMoment beam subs = 
  let m = 0.1 * beam.r
      parallel = { x = beam.r / 2, y = 0, m = m, localMoment = (1/12) * m * beam.r * beam.r }
  in ParallelAxis parallel :: concat subs

momentMapVec : (Vec2 -> Vec2) -> Moment -> Moment
momentMapVec f m = case m of
  Point pm -> 
    let pmv = extractVec pm
        pmf = f pmv
    in Point <| { pm | x <- pmf.x, y <- pmf.y }
  ParallelAxis pa -> 
    let pav = extractVec pa
        paf = f pav
    in ParallelAxis <| { pa | x <- paf.x, y <- paf.y }

translateMoment : Attach -> Moment -> Moment
translateMoment a m = case m of
  Point pm -> Point <| translateAttach a pm
  ParallelAxis pa -> ParallelAxis <| translateAttach a pa

attachMoment : Attach -> List Moment -> List Moment
attachMoment attach = map (translateMoment attach)

structureMoments : Structure -> List Moment
structureMoments = foldTagTree partMoment beamMoment attachMoment

momentContrib : Moment -> Float
momentContrib m = case m of
  Point pm -> 
    let r = magnitude pm
    in pm.m * r * r
  ParallelAxis pa ->
    let r = magnitude pa
    in pa.m * r * r + pa.localMoment

rotInertiaContrib : PointMass -> Float
rotInertiaContrib pm =
  let r = magnitude pm
  in pm.m * r * r

rotInertia : Structure -> Float
rotInertia structure = 
  sum <<
  map momentContrib <<
  map (momentMapVec (subVec <| centerOfMass structure)) <<
  structureMoments <|
  structure

genEntityCache : Structure -> EntityCache
genEntityCache s = EntityCache
  s
  (centerOfMass s)
  (totalMass s)
  (rotInertia s)

{- Thrust -}

partThrusts : List EngineConfig -> Part -> List Thrust
partThrusts ecs part = 
  case part of
    (Engine engine) -> 
      if any (\ec -> ec == engine.config) ecs
      then let force = -8 * engine.r
           in [ { disp = origin, force = { x = force, y = 0 } } ]
      else [] 
    _ -> []

beamThrusts : Beam -> List (List Thrust) -> List Thrust
beamThrusts = cnst concat

attachThrusts : Attach -> List Thrust -> List Thrust
attachThrusts attach ts = 
  let offsetDisp t = { t | disp <- addVec { x = attach.offset, y = 0 } t.disp }
      rotateDisp t = { t | disp <- rotVec attach.theta t.disp }
      rotateForce t = { t | force <- rotVec attach.theta t.force }
  in map (offsetDisp << rotateDisp << rotateForce) <| ts

structureThrusts : List EngineConfig -> Structure -> List Thrust
structureThrusts ec = foldTagTree (partThrusts ec) beamThrusts attachThrusts

{- Entity Physics -}

netForce : List EngineConfig -> EntityCache -> Vec2
netForce controls cache = 
  sumVec << map .force <| structureThrusts controls cache.structure

netAcceleration : List EngineConfig -> EntityCache -> Vec2
netAcceleration controls cache = 
  scaleVec (1 / cache.totalMass) <| netForce controls cache

torqueMag : Thrust -> Float
torqueMag t = crossVecMag t.disp t.force

netTorque : List EngineConfig -> EntityCache -> Float
netTorque controls cache =
  let com = cache.comOffset
      thrusts = structureThrusts controls cache.structure
      center thrust = { thrust | disp <- subVec com thrust.disp }
      torqueForces = map center thrusts
  in sum << map torqueMag <| torqueForces 

netRotAcceleration : List EngineConfig -> EntityCache -> Float
netRotAcceleration controls cache = 
  netTorque controls cache / cache.rotInertia

netDelta : List EngineConfig -> EntityCache -> MotionDelta
netDelta controls cache = MotionDelta
  (netAcceleration controls cache)
  (netRotAcceleration controls cache)

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

updateBrakes : EntityCache -> MotionState -> MotionState
updateBrakes cache state =
  let brakeTheta = clamp (-0.001) 0.001 state.omega
      -- TODO clamp the magnitude of v, rather than the components of v individually?
      brakeVelX = clamp (-0.1) 0.1 state.v.x
      brakeVelY = clamp (-0.1) 0.1 state.v.y
      fakeDelta = { a = rotVec -state.pos.theta { x = -brakeVelX, y = -brakeVelY }, alpha = -brakeTheta }
  in updateMotion cache fakeDelta state

updateMotion : EntityCache -> MotionDelta -> MotionState -> MotionState
updateMotion cache delta state = 
  let newOmega = state.omega + delta.alpha
      oldTheta = state.pos.theta
      newTheta = oldTheta + state.omega
      absA = rotVec oldTheta delta.a
      newV = addVec absA state.v
      midV = midVec state.v newV
      oldRootPos = state.pos
      oldComOffset = rotVec oldTheta cache.comOffset
      oldComPos = addVec oldComOffset oldRootPos
      newComPos = addVec midV <| { oldComPos | theta <- newTheta }
      newComOffset = rotVec newTheta cache.comOffset
      newRootPos = subVec newComOffset newComPos
  in { state | pos <- newRootPos, v <- newV, omega <- newOmega }
