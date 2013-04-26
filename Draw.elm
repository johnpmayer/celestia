module Draw where

import Graphics.Collage
import Graphics.Geometry
import Graphics.Matrix as M

import Types

-- Import Types Aliases
type Attach =
  { offset : Float
  , theta : Float }
type Position =
  { x     : Float
  , y     : Float
  , theta : Float }

drawPart : Part -> Form
drawPart part =
  case part of
    (Brain size) -> 
      filled blue $ circle size.r 
    (FuelTank size) -> 
      filled green $ rect size.w size.l 
    (Engine size) -> 
      rotate 0.5 . filled red $ ngon 3 size.r 

drawAttachment : (Attach, Structure) -> Form
drawAttachment ({offset,theta}, structure) =
  let modelM = M.rotate theta (M.move 0 offset (M.identity))
      subForm = drawStructure structure
  in groupTransform modelM [subForm]

drawBeam : Float -> [(Attach, Structure)] -> Form
drawBeam length attachments =
  let width = length * 0.05
      beamForm = filled gray $ rect width length
  in group (beamForm :: map drawAttachment attachments) 

drawStructure : Structure -> Form
drawStructure structure =
  case structure of
    Module part -> drawPart part
    Beam length attachments -> drawBeam length attachments

