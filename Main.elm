
{-
    Copyright (c) John P Mayer, Jr 2013

    This file is part of celestia.

    celestia is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    celestia is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with celestia.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

import Char (toCode)

import Keyboard as K
import Mouse as M

import open Build
import open Draw 
import open Physics 
import open Types 
import open Utils
import open Public.Vec2.Vec2

attitude : EngineConfig -> Structure
attitude ec = beam { r=10 } <|
  [ ( { offset=10, theta=0 }, 
      part <| Engine {r=4, config=ec} ) ]

reverseL : Structure
reverseL = beam { r=10 } <|
  [ ( { offset=10, theta=(-pi/2) }, 
      part <| Engine {r=4, config=Reverse} ) ]

reverseR : Structure
reverseR = beam { r=10 } <|
  [ ( { offset=10, theta=(pi/2) }, 
      part <| Engine {r=4, config=Reverse} ) ]

mods : [(Attach, Structure)]
mods = 
  [ ({ offset=6, theta=(3*pi/2) }, reverseL)
  , ({ offset=6, theta=(pi/2) }, reverseR)
  , ({ offset=42, theta=(3*pi/2) }, attitude TurnLeft)
  , ({ offset=42, theta=(pi/2) }, attitude TurnRight)
  , ({ offset=0, theta=0 }, part <| Brain {r=7})
  , ({ offset=25, theta=0 }, part <| FuelTank {l=30,w=14})
  , ( { offset=50, theta=0 }, 
      part <| Engine {r=13, config=Forward}) ]

simpleShip : Structure
simpleShip = beam { r=50 } mods

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

canvasWH : (Int,Int)
canvasWH = (400,300)

gamePointer : Signal (Int,Int)
gamePointer = (convertPos canvasWH) <~ M.position

space' : (Int,Int) -> BuildMode -> Form -> Element
space' point mode objects = spaceBlack canvasWH
    [ (construct point mode)
    , objects
    , drawBuildArea mode
    ]

space : Signal Element
space = space' <~ gamePointer ~ mode ~ gameForms

main : Signal Element
main = combineSElems <|
  [ space 
  , constant . plainText <| "Ship position"
  , asText <~ state
  , asText <~ K.keysDown
  , asText <~ brakes
  , asText <~ K.space
  , asText <~ ((minimumDist origin (fromIntPair (50,0))) <~ (fromIntPair <~ gamePointer))
  , constant . asText . labelBeams <| simpleShip
  ]

