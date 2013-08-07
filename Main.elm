module Main where

import Keyboard (wasd)

import open Types 
import Draw (drawStructure)
import Physics (structureCOM, structureThrusts)

attitudeL : Structure
attitudeL = Node { l=10 } <|
  [ ( { offset=10, theta=0 }, 
      Leaf <| Engine {r=4, config=TurnLeft} ) ]

attitudeR : Structure
attitudeR = Node { l=10 } <| 
  [ ( { offset=10, theta=0 }, 
      Leaf <| Engine {r=4, config=TurnRight} ) ]

mods : [(Attach, Structure)]
mods = 
  [ ({ offset=43, theta=(3*pi/2) }, attitudeL)
  , ({ offset=43, theta=(pi/2) }, attitudeR)
  , ({ offset=35, theta=(3*pi/2) }, attitudeL)
  , ({ offset=35, theta=(pi/2) }, attitudeR)
  , ({ offset=0, theta=0 }, Leaf <| Brain {r=7})
  , ({ offset=20, theta=0 }, Leaf <| FuelTank {l=20,w=14})
  , ( { offset=50, theta=0 }, 
      Leaf <| Engine {r=10, config=Forward}) ]

simpleShip : Structure
simpleShip = Node { l=50 } mods

control : { x:Int, y:Int } -> [ EngineConfig ]
control input = 
  (if input.y > 0 then [ Forward ] else []) ++
  (if input.x > 0 then [ TurnRight ] else []) ++
  (if input.x < 0 then [ TurnLeft ] else [])

position : Signal Position
position = constant <|
  { x = 50
  , y = 50
  , theta = 0 }

ship : Signal Form
ship = (\ec -> drawStructure ec simpleShip) <~ (control <~ wasd)

space : Signal Element
space = (\ship position ->
  collage 400 300 <|
    [ filled black <| rect 400 300
    , move (position.x, position.y) . rotate (position.theta) <| ship
    ]) <~ ship ~ position

main : Signal Element
main = (\ses -> flow down <~ combine ses) <|
  [ space 
  , constant . plainText <| "Ship position"
  , asText <~ position
  , constant . plainText <| "Ship Controls"
  , asText . control <~ wasd
  , constant . plainText <| "Ship thrusts (local to structure)"
  , asText <~ (structureThrusts <~ (control <~ wasd) ~ (constant simpleShip))
  ]
