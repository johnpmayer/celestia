module Draw where

import Types

-- These need to be copied into every file now (12 lines)
type Attach =
  { offset : Float
  , theta : Float }
type Position =
  { x     : Float
  , y     : Float
  , theta : Float }

angleToTurn : Float -> Float
angleToTurn theta = theta / (2 * pi)

drawPart : Position -> Part -> [Form]
drawPart pos part =
  let local = case part of
    (Brain size) -> 
      filled blue $ circle size.r (pos.x,pos.y)
    (FuelTank size) -> 
      filled green $ rect size.w size.l (pos.x,pos.y)
    (Engine size) -> 
      rotate 0.5 .
      filled red $ ngon 3 size.l (pos.x,pos.y)
  in  [ rotate (angleToTurn pos.theta) local ]

drawBeam : Position -> Float -> [[Form]] -> [Form]
drawBeam pos beamL subStructureForms =
  let beamCenter = modPosition pos {  offset=(beamL/2),
                                      theta=pos.theta }
      beamW = beamL * 0.05
      beamForm = 
        rotate (angleToTurn pos.theta) .
        filled black $ 
          rect beamL beamW (beamCenter.x,beamCenter.y)
  in beamForm :: concat subStructureForms

drawStructure : Position -> Structure -> [Form]
drawStructure pos structure =
  fold drawPart drawBeam pos structure

