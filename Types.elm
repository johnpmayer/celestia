module Types where

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

data Structure = Node Size Part 

data Part = Brain
          | FuelTank
          | Engine
          | Beam [(Attach, Structure)]
