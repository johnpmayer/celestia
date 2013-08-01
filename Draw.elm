module Draw where

import Graphics.Collage
import Matrix2D (Matrix2D)
import Matrix2D as M

import Types (Part, Brain, FuelTank, Engine, Structure, Beam, Module, Attach)

translation : Float -> Float -> Matrix2D
translation = M.matrix 1 0 0 1

drawPart : Part -> Form
drawPart part =
  case part of
    (Brain size) -> 
      filled blue <| circle size.r 
    (FuelTank size) -> 
      filled green <| rect size.w size.l 
    (Engine size) -> 
      rotate 0.5 . filled red <| ngon 3 size.r 

drawAttachment : (Attach, Structure) -> Form
drawAttachment ({offset,theta}, structure) =
  let modelM = M.multiply (M.rotation theta) (translation 0 offset)
      subForm = drawStructure structure
  in groupTransform modelM [subForm]

drawBeam : Float -> [(Attach, Structure)] -> Form
drawBeam length attachments =
  let width = length * 0.05
      beamForm = filled gray <| rect width length
  in group (beamForm :: map drawAttachment attachments) 

drawStructure : Structure -> Form
drawStructure structure =
  case structure of
    Module part -> drawPart part
    Beam length attachments -> drawBeam length attachments

