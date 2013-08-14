module Main where

import Keyboard 
import Mouse

import open Build
import open Draw 
import open Types 
import open Physics 

attitude : EngineConfig -> Structure
attitude ec = Node { r=10 } <|
  [ ( { offset=10, theta=0 }, 
      Leaf <| Engine {r=4, config=ec} ) ]

reverseL : Structure
reverseL = Node { r=10 } <|
  [ ( { offset=10, theta=(-pi/2) }, 
      Leaf <| Engine {r=4, config=Reverse} ) ]

reverseR : Structure
reverseR = Node { r=10 } <|
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
simpleShip = constant <| Node { r=50 } mods

startPos : MotionState
startPos = { pos = { x = 0, y = 0, theta = 0 }, v = { x = 0, y = 0 }, omega = 0 }

control : { x:Int, y:Int } -> [ EngineConfig ]
control input = 
  (if input.y < 0 then [ Reverse ] else []) ++
  (if input.y > 0 then [ Forward ] else []) ++
  (if input.x > 0 then [ TurnRight ] else []) ++
  (if input.x < 0 then [ TurnLeft ] else [])

engines : Signal [ EngineConfig ]
engines = control <~ Keyboard.wasd

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

mode : Signal BuildMode
mode = foldp updateMode Inactive Keyboard.lastPressed

construct : (Int,Int) -> BuildMode -> Form
construct (x,y) mode = move (toFloat x, toFloat y) <| blueprint mode

canvasW : Int
canvasW = 400

canvasH : Int
canvasH = 300

convertPos : (Int,Int) -> (Int,Int)
convertPos (x,y) = (x - div canvasW 2, div canvasH 2 - y)

space : Signal Element
space = (\ship state mouse mode ->
  collage canvasW canvasH <|
    [ filled black <| rect (toFloat canvasW) (toFloat canvasH)
    , construct (convertPos mouse) mode
    , move (state.pos.x, state.pos.y) . rotate (state.pos.theta) <| ship
    ]) <~ ship ~ state ~ Mouse.position ~ mode

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
  , asText <~ mode
  ]
