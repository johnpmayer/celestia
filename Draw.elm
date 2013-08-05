module Draw where

import Graphics.Collage
import Matrix2D (Matrix2D)
import Matrix2D as M

import Types (Part, Brain, FuelTank, Engine, Structure, Beam, Module, Attach, EngineConfig)

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

drawAttachment : [EngineConfig] -> (Attach, Structure) -> Form
drawAttachment ec ({offset,theta}, structure) =
  let modelM = M.multiply (translation 0 offset) (M.rotation theta)
      subForm = drawStructure ec structure
  in groupTransform modelM [subForm]

drawBeam : [EngineConfig] -> Float -> [(Attach, Structure)] -> Form
drawBeam ec length attachments =
  let width = length * 0.05
      beamForm = move (0, length * 0.5) . filled gray <| rect width length
  in group (beamForm :: map (drawAttachment ec) attachments) 

drawStructure : [EngineConfig] -> Structure -> Form
drawStructure ec structure =
  case structure of
    Module part -> drawPart ec part
    Beam length attachments -> drawBeam ec length attachments

