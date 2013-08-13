module Main where

import Keyboard (wasd)

import open Types 
import open Draw 
import open Physics 

attitude : EngineConfig -> Structure
attitude ec = Node { l=10 } <|
  [ ( { offset=10, theta=0 }, 
      Leaf <| Engine {r=4, config=ec} ) ]

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
  [ ({ offset=6, theta=(3*pi/2) }, reverseL)
  , ({ offset=6, theta=(pi/2) }, reverseR)
  , ({ offset=42, theta=(3*pi/2) }, attitude TurnLeft)
  , ({ offset=42, theta=(pi/2) }, attitude TurnRight)
  , ({ offset=0, theta=0 }, Leaf <| Brain {r=7})
  , ({ offset=25, theta=0 }, Leaf <| FuelTank {l=30,w=14})
  , ( { offset=50, theta=0 }, 
      Leaf <| Engine {r=13, config=Forward}) ]

simpleShip : Signal Structure
simpleShip = constant <| Node { l=50 } mods

startPos : MotionState
startPos = { pos = { x = 0, y = 0, theta = 0 }, v = { x = 0, y = 0 }, omega = 0 }

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

position : [Signal Element] -> Signal Element
position ses = flow down <~ combine ses

main : Signal Element
main = position <|
  [ space 
  , constant . plainText <| "Ship position"
  , asText <~ state
  , constant . plainText <| "Ship center of mass (local to structure)"
  , (asText . centerOfMass) <~ simpleShip
  , constant . plainText <| "Ship thrusts (local to structure)"
  , asText <~ (netDelta <~ (engines) ~ simpleShip)
  ]
