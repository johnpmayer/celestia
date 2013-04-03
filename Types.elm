module Types where

--import List

-- These need to be copied into every file now (12 lines)
type Attach =
  { offset : Float
  , theta : Float }
type Position =
  { x     : Float
  , y     : Float
  , theta : Float }

modPosition : Position -> Attach -> Position
modPosition beamPos modAtt =
  let modX = beamPos.x + modAtt.offset * (cos beamPos.theta)
      modY = beamPos.y + modAtt.offset * (sin beamPos.theta)
      modTheta = beamPos.theta + modAtt.theta
  in { x=modX, y=modY, theta=modTheta }

data Structure = Module Part
               | Beam Float [(Attach, Structure)]

data EngineConfig = Thrust 
                  | TurnLeft 
                  | TurnRight

data Part = Brain { r:Float }
          | FuelTank { l:Float, w:Float }
          | Engine { r:Float, config:EngineConfig }

fold : 
  (Position -> Part -> a) -> 
  (Position -> Float -> [a] -> a) -> 
  Position -> Structure -> a
fold fPart fBeam pos structure = 
  case structure of
    (Module part) -> fPart pos part
    (Beam length attachments) ->
      let foldAttachment (attach, structure) =
        let subPos = modPosition pos attach
        in fold fPart fBeam subPos structure
      in fBeam pos length $ map foldAttachment attachments

getParts : Position -> Structure -> [(Position, Part)]
getParts =
  let fPart pos part = [(pos,part)]
      fBeam _ _ subPartLists = concat subPartLists
  in fold fPart fBeam
