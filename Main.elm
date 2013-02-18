module Main where

import Draw
import Types

-- These need to be copied into every file now (12 lines)
type Attach =
  { offset : Float
  , facing : Float }
type Position =
  { x     : Float
  , y     : Float
  , theta : Float }

attitudeL : Structure
attitudeL = Beam 10 $
  [ ( { offset=10, facing=0 }, 
      Module $ Engine {l=5,w=5,config=TurnLeft} ) ]

attitudeR : Structure
attitudeR = Beam 10 $ 
  [ ( { offset=10, facing=0 }, 
      Module $ Engine {l=5,w=5,config=TurnRight} ) ]

mods : [(Attach, Structure)]
mods = 
  [ ({ offset=0, facing=0 }, Module $ Brain {r=10})
  , ({ offset=25, facing=0 }, Module $ FuelTank {l=20,w=14})
  , ( { offset=50, facing=0 }, 
      Module $ Engine {l=10,w=10,config=Thrust})
  , ({ offset=45, facing=(pi/4) }, attitudeL)
  , ({ offset=45, facing=(3*pi/4) }, attitudeR) ]

simpleShip : Structure
simpleShip = Beam 50 mods

main : Element
main = 
  collage 400 300 $ 
    drawStructure {x=50,y=50,theta=0} simpleShip
