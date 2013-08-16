module Main where

import Char (toCode)

import Keyboard as K
import Mouse as M

import open Build
import open Draw 
import open Physics 
import open Types 
import open Vec2

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

simpleShip : Structure
simpleShip = Node { r=50 } mods

startPos : MotionState
startPos = { pos = { x = 0, y = 0, theta = 0 }, v = { x = 0, y = 0 }, omega = 0 }

control : { x:Int, y:Int } -> [ EngineConfig ]
control input = 
  (if input.y < 0 then [ Reverse ] else []) ++
  (if input.y > 0 then [ Forward ] else []) ++
  (if input.x > 0 then [ TurnRight ] else []) ++
  (if input.x < 0 then [ TurnLeft ] else [])

engines : Signal [ EngineConfig ]
engines = control <~ K.wasd

brakes : Signal Bool
brakes = K.isDown <| toCode 'x'

delta : Signal MotionDelta
delta = sampleOn (fps 30) <| netDelta <~ engines ~ (constant simpleShip)

state : Signal MotionState
state = foldp updateMotion startPos ((,) <~ brakes ~ delta)

myShip : Signal Entity
myShip = (\engines state structure -> { controls = engines, motion = state, structure = structure }) <~ engines ~ state ~ (constant simpleShip)

gameObjects : Signal [Entity]
gameObjects = combine <| 
  constant { controls = [], motion = { pos = { x=60,y=50,theta=1}, v = {x=0,y=0}, omega=0}, structure = simpleShip } ::
  constant { controls = [], motion = { pos = { x=-80,y=40,theta=4}, v = {x=0,y=0}, omega=0}, structure = simpleShip } ::
  constant { controls = [], motion = { pos = { x=120,y=-20,theta=3}, v = {x=0,y=0}, omega=0}, structure = simpleShip } ::
  [ myShip ]

clock : Signal Time
clock = fps 30

frameStamp : Signal Time
frameStamp = fst <~ timestamp clock

camera : Signal Vec2
camera = (extractVec . .pos) <~ state

gameForms : Signal Form
gameForms = 
  (\time es camera -> 
    let modelM = vecTranslate <| negVec camera
        forms = map (drawEntity time) es
    in groupTransform modelM forms)
    <~ frameStamp ~ gameObjects ~ camera

mode : Signal BuildMode
mode = foldp updateMode Inactive K.lastPressed

construct : (Int,Int) -> BuildMode -> Form
construct (x,y) mode = move (toFloat x, toFloat y) <| blueprint mode

canvasW : Int
canvasW = 400

canvasH : Int
canvasH = 300

convertPos : (Int,Int) -> (Int,Int)
convertPos (x,y) = (x - div canvasW 2, div canvasH 2 - y)

space' : (Int,Int) -> BuildMode -> Form -> Element
space' mouse mode objects = 
  (collage canvasW canvasH) <|
    [ (filled black <| rect (toFloat canvasW) (toFloat canvasH))
    , (construct (convertPos mouse) mode)
    , objects
    , drawBuildArea mode
    ]

space : Signal Element
space = space' <~ M.position ~ mode ~ gameForms

position : [Signal Element] -> Signal Element
position ses = flow down <~ combine ses

main : Signal Element
main = position <|
  [ space 
  , constant . plainText <| "Ship position"
  , asText <~ state
  , asText <~ K.keysDown
  , asText <~ brakes
  , asText <~ K.space
  , asText <~ ((minimumDist origin (fromIntPair (50,0))) <~ ((fromIntPair . convertPos) <~ M.position))
  ]

