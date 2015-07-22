
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

import Char exposing (toCode)

import Keyboard as K
import Mouse as M
import Signal exposing (..)

import Draw exposing (..)
import Physics exposing (..)
import Types exposing (..)
import Utils exposing (..)
import Data.Vec2 exposing (..)

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

mods : List (Attach, Structure)
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

control : { x:Int, y:Int } -> List EngineConfig 
control input = 
  (if input.y < 0 then [ Reverse ] else []) ++
  (if input.y > 0 then [ Forward ] else []) ++
  (if input.x > 0 then [ TurnRight ] else []) ++
  (if input.x < 0 then [ TurnLeft ] else [])

engines : Signal (List EngineConfig) 
engines = control <~ K.wasd

brakes : Signal Bool
brakes = K.isDown <| toCode 'x'

canvasWH : (Int,Int)
canvasWH = (400,300)

gamePointer : Signal (Int,Int)
gamePointer = (convertPos canvasWH) <~ M.position
