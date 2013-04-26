module Types where

type Attach =
  { offset : Float
  , theta : Float }
type Position =
  { x     : Float
  , y     : Float
  , theta : Float }

data Structure = Module Part
               | Beam Float [(Attach, Structure)]

data EngineConfig = Thrust 
                  | TurnLeft 
                  | TurnRight

data Part = Brain { r:Float }
          | FuelTank { l:Float, w:Float }
          | Engine { r:Float, config:EngineConfig }

