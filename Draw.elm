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
    (Engine engine) -> 
      group <|
        (rotate pi . filled red <| ngon 3 engine.r) ::
        (if any (\ec -> ec == engine.config) ecs
        then [ move ((0.7*engine.r),0) . rotate pi . filled yellow <| ngon 3 (engine.r * 0.7) ]
        else [])

drawAttach : Attach -> Form -> Form
drawAttach {offset,theta} subForm =
  let modelM = M.multiply (translation offset 0) (M.rotation theta)
  in groupTransform modelM [subForm]

drawBeam : Beam -> [Form] -> Form
drawBeam beam subForms =
  let l = beam.l
      w = l * 0.05
      beamForm = move (l * 0.5, 0) . filled gray <| rect l w
  in group (beamForm :: subForms)

drawStructure : [EngineConfig] -> Structure -> Form
drawStructure ec =
  foldTagTree (drawPart ec) drawBeam drawAttach

