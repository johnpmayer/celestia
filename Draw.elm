module Draw where

import Types
import List

-- These need to be copied into every file now (12 lines)
type Size = 
  { length : Float
  , ratio  : Float }
type Attach =
  { offset : Float
  , facing : Float }
type Ship = 
  { x      : Float 
  , y      : Float 
  , facing : Float
  , struct : Structure }

-- local type
type Placement =
  { x      : Float
  , y      : Float
  , facing : Float }


drawAttachedPart : Placement -> (Attach,Structure) -> [Form]
drawAttachedPart beamPos (attach, struct) =
  let theta  = 2 * pi * beamPos.facing
      x      = attach.offset * cos theta
      y      = attach.offset * sin theta
      facing = attach.facing
  in drawPart { x=x, y=y, facing=facing } struct

drawPart : Placement -> Structure -> [Form]
drawPart pos (Node size part) = 
  let local = case part of
    Brain        -> [ filled blue $ 
                      circle size.length (0,0) ]
    FuelTank     -> [ filled green $ 
                      rect size.length (size.length * size.ratio) (0,0) ]
    Engine       -> [ rotate 0.5 . filled red $ 
                      ngon 3 size.length (0,0) ]
    (Beam pairs) -> (( move (size.length/2) 0 . outlined gray $
                       rect size.length (size.length * size.ratio) (0,0))
                       :: concatMap (drawAttachedPart pos) pairs)
  in map ((move pos.x pos.y) . (rotate pos.facing)) local

drawShip : Ship -> [Form]
drawShip ship = 
  let placement = { x=ship.x, y=ship.y, facing=ship.facing }
  in drawPart placement ship.struct
