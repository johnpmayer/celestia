module Draw where

import Graphics.Collage

import Matrix2D (Matrix2D)
import Matrix2D as M

import open Physics
import open Types 
import open Vec2

translation : Float -> Float -> Matrix2D
translation = M.matrix 1 0 0 1

drawPart : Time -> [EngineConfig] -> Part -> Form
drawPart noise ecs part =
  let hashedNoise = mod (truncate noise) 150
      gasColor = rgb 255 (255 - hashedNoise) 0
  in case part of
    (Brain size) -> 
      filled blue <| circle size.r 
    (FuelTank size) -> 
      filled green <| rect size.l size.w 
    (Engine engine) -> 
      group <|
        (rotate pi . filled red <| ngon 3 engine.r) ::
        (if any (\ec -> ec == engine.config) ecs
        then [ move ((0.4 * engine.r), 0) . rotate pi . filled gasColor <| ngon 3 (engine.r * 0.8) ]
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

drawStructure : Time -> [EngineConfig] -> Structure -> Form
drawStructure noise ec structure =
  let comOffset = scaleVec -1 <| centerOfMass structure
  in groupTransform (translation comOffset.x comOffset.y) <|
    [ foldTagTree (drawPart noise ec) drawBeam drawAttach structure ]

