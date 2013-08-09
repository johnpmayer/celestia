module Main where

import Keyboard (wasd)

import open Types 
import open Draw 
import open Physics 

attitudeL : Structure
attitudeL = Node { l=10 } <|
  [ ( { offset=10, theta=0 }, 
      Leaf <| Engine {r=4, config=TurnLeft} ) ]

attitudeR : Structure
attitudeR = Node { l=10 } <| 
  [ ( { offset=10, theta=0 }, 
      Leaf <| Engine {r=4, config=TurnRight} ) ]

reverseL : Structure
reverseL = Node { l=10 } <|
  [ ( { offset=10, theta=(-pi/2) }, 
      Leaf <| Engine {r=4, config=Reverse} ) ]

reverseR : Structure
reverseR = Node { l=10 } <|
  [ ( { offset=10, theta=(pi/2) }, 
      Leaf <| Engine {r=4, config=Reverse} ) ]

mods : [(Attach, Structure)]
mods = 
  [ ({ offset=50, theta=(3*pi/2) }, reverseL)
  , ({ offset=50, theta=(pi/2) }, reverseR)
  , ({ offset=42, theta=(3*pi/2) }, attitudeL)
  , ({ offset=42, theta=(pi/2) }, attitudeR)
  , ({ offset=34, theta=(3*pi/2) }, attitudeL)
  , ({ offset=34, theta=(pi/2) }, attitudeR)
  , ({ offset=0, theta=0 }, Leaf <| Brain {r=7})
  , ({ offset=20, theta=0 }, Leaf <| FuelTank {l=20,w=14})
  , ( { offset=50, theta=0 }, 
      Leaf <| Engine {r=10, config=Forward}) ]

simpleShip : Signal Structure
simpleShip = constant <| Node { l=50 } mods

startPos : MotionState
startPos = { pos = { x = 50, y = 50, theta = 0 }, v = { x = 0, y = 0 }, omega = 0 }

control : { x:Int, y:Int } -> [ EngineConfig ]
control input = 
  (if input.y < 0 then [ Reverse ] else []) ++
  (if input.y > 0 then [ Forward ] else []) ++
  (if input.x > 0 then [ TurnRight ] else []) ++
  (if input.x < 0 then [ TurnLeft ] else [])

engines : Signal [ EngineConfig ]
engines = control <~ wasd

delta : Signal MotionDelta
delta = sampleOn (fps 30) <| netDelta <~ engines ~ simpleShip

state : Signal MotionState
state = foldp updateMotion startPos delta

clock : Signal Time
clock = fps 30

frameStamp : Signal Time
frameStamp = fst <~ timestamp clock

ship : Signal Form
ship = (drawStructure) <~ frameStamp ~ engines ~ simpleShip

space : Signal Element
space = (\ship state ->
  collage 400 300 <|
    [ filled black <| rect 400 300
    , move (state.pos.x, state.pos.y) . rotate (state.pos.theta) <| ship
    ]) <~ ship ~ state

main : Signal Element
main = (\ses -> flow down <~ combine ses) <|
  [ space 
  , constant . plainText <| "Ship position"
  , asText <~ state
  , constant . plainText <| "Ship Controls"
  , asText <~ engines
  , constant . plainText <| "Ship thrusts (local to structure)"
  , asText <~ (structureThrusts <~ (engines) ~ simpleShip)
  , asText <~ (netAcceleration <~ (engines) ~ simpleShip)
  , asText <~ frameStamp
  ]
