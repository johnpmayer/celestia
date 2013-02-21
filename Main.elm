module Main where

import Draw
import Types

-- These need to be copied into every file now (12 lines)
type Attach =
  { offset : Float
  , theta : Float }
type Position =
  { x     : Float
  , y     : Float
  , theta : Float }

attitudeL : Structure
attitudeL = Beam 10 $
  [ ( { offset=10, theta=0 }, 
      Module $ Engine {r=5,config=TurnLeft} ) ]

attitudeR : Structure
attitudeR = Beam 10 $ 
  [ ( { offset=10, theta=0 }, 
      Module $ Engine {r=5,config=TurnRight} ) ]

mods : [(Attach, Structure)]
mods = 
  [ ({ offset=45, theta=(pi/2) }, attitudeL)
  , ({ offset=45, theta=(3*pi/2) }, attitudeR)
  , ({ offset=0, theta=0 }, Module $ Brain {r=7})
  , ({ offset=25, theta=0 }, Module $ FuelTank {l=20,w=14})
  , ( { offset=50, theta=0 }, 
      Module $ Engine {r=10,config=Thrust}) ]

simpleShip : Structure
simpleShip = Beam 50 mods

main : Element
main =  
  collage 400 300 $ 
    drawStructure {x=50,y=50,theta=(pi/4)} simpleShip
