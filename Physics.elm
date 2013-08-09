module Physics where

import List
import open Vec2
import open Types

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

{- Thrust -}

partThrusts : [EngineConfig] -> Part -> [Thrust]
partThrusts ecs part = 
  case part of
    (Engine engine) -> 
      if any (\ec -> ec == engine.config) ecs
      then let force = -0.15 * engine.r
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

-- structureRotInertia

-- totalTorque
