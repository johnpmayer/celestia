
module Types where

type Dimensions = 
    { length : Float
    , width : Float }

data Part = FuelTank Dimensions
          | Engine Dimensions
          | Strut Dimensions

type Position =
    { offsetX : Float
    , offsetY : Float
    , orientation : Float }

data PartTree = PT Position Part [PartTree]

data SpaceCraft = Brain [PartTree]

