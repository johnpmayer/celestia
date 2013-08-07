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

{-
structureCOM : Position -> Structure -> Position
structureCOM rootPos structure =
  let parts = getParts rootPos structure
      masses = map (\(_,part) -> partMass part) parts
      totalM = sum masses
      contribs = 
        map (\(pos,part) ->
          let mass = partMass part
          in (pos.x * mass, pos.y * mass))
      addcontrib (mx,my) (accx,accy) =
        (mx + accx, my + accy)
      (xM,yM) = foldr addcontrib (0,0) contribs
  in { x=xM / totalM
     , y=yM / totalM
     , theta=rootPos.theta }

partThrust : Part -> { x:Float, y:Float }
partThrust p = { x=0, y=0 }
-}

-- totalThrust

-- totalAcceleration

-- structureRotInertia

-- totalTorque
