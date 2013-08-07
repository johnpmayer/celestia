module Draw where

import Graphics.Collage

import Matrix2D (Matrix2D)
import Matrix2D as M

import open Types 

translation : Float -> Float -> Matrix2D
translation = M.matrix 1 0 0 1

drawPart : [EngineConfig] -> Part -> Form
drawPart ecs part =
  case part of
    (Brain size) -> 
      filled blue <| circle size.r 
    (FuelTank size) -> 
      filled green <| rect size.w size.l 
    (Engine size) -> 
      group <|
        (rotate 0.5 . filled red <| ngon 3 size.r) ::
        (if any (\ec -> ec == size.config) ecs
        then [ move (0,(0.7*size.r)) . rotate 0.5 . filled yellow <| ngon 3 (size.r * 0.7) ]
        else [])

drawAttach : Attach -> Form -> Form
drawAttach {offset,theta} subForm =
  let modelM = M.multiply (translation 0 offset) (M.rotation theta)
  in groupTransform modelM [subForm]

drawBeam : Beam -> [Form] -> Form
drawBeam beam subForms =
  let l = beam.l
      w = l * 0.05
      beamForm = move (0, l * 0.5) . filled gray <| rect w l
  in group (beamForm :: subForms)

drawStructure : [EngineConfig] -> Structure -> Form
drawStructure ec =
  foldTagTree (drawPart ec) drawBeam drawAttach

