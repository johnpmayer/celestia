module Physics where

import List
import open Types

{- Center of Mass -}

sumMasses : [PointMass] -> Float
sumMasses = sum . map (\pm -> pm.m)

massContrib : PointMass -> Vec2
massContrib pm = 
  { x = pm.x * pm.m
  , y = pm.y * pm.m
  }

partMasses : Part -> [PointMass]
partMasses part = 
  let m = case part of
    (Brain size)    -> 3 * size.r * size.r
    (FuelTank size) -> 1.5 * size.l * size.w
    (Engine size)   -> 2 * size.r * size.r
  in [{ x=0, y=0, m=m }]

beamMasses : Beam -> [[PointMass]] -> [PointMass]
beamMasses beam subMasses = concat subMasses

attachMasses : Attach -> [PointMass] -> [PointMass]
attachMasses attach = map (translateAttach attach)

structureMasses : Structure -> [PointMass]
structureMasses = foldTagTree partMasses beamMasses attachMasses

centerOfMass : Structure -> Vec2
centerOfMass structure = 
  let masses = structureMasses structure
      totalMass = sumMasses masses
      totalPointMass = 
        foldl addVec origin . map massContrib <| masses 
  in scaleVec (1 / totalMass) totalPointMass

partThrust : Part -> Vec2
partThrust p = { x=0, y=0 }

-- totalThrust

-- totalAcceleration

-- structureRotInertia

-- totalTorque
