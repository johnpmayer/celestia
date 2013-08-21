module Draw where

import Graphics.Collage

import Transform2D (Transform2D)
import Transform2D as T

import open Physics
import open Public.TagTree.TagTree
import open Types 
import open Public.Vec2.Vec2

translation : Float -> Float -> Transform2D
translation = T.matrix 1 0 0 1

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
  let modelM = T.multiply (translation offset 0) (T.rotation theta)
  in groupTransform modelM [subForm]

drawBeam : Beam -> [Form] -> Form
drawBeam beam subForms =
  let l = beam.r
      w = l * 0.05
      beamForm = move (l * 0.5, 0) . filled (gray 0.5) <| rect l w
  in group (beamForm :: subForms)

drawStructure : Time -> [EngineConfig] -> Structure -> Form
drawStructure noise ec structure =
  foldTagTree (drawPart noise ec) drawBeam drawAttach structure

drawEntity : Time -> Entity -> Form
drawEntity noise { controls, motion, structure } = 
  let comOffset = scaleVec -1 <| centerOfMass structure
      comM = translation comOffset.x comOffset.y
      rotM = T.rotation motion.pos.theta
      moveM = translation motion.pos.x motion.pos.y
      modelM = T.multiply moveM (T.multiply rotM comM)
  in groupTransform modelM <| [ drawStructure noise controls structure ]

drawBuildArea : BuildMode -> Form
drawBuildArea mode = case mode of
  Inactive -> group []
  _        -> filled (rgba 255 255 0 0.2) <| oval 200 200
