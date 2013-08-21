
module Step where

import Dict (Dict)

import Trigger (Trigger, Modal)
import Types (Entity)

import Public.State.State as ST

import open Utils

type GameState = { entities : Dict Int Entity, mode : Mode }

type Mode = { pause : Bool, build : BuildMode }

data BuildMode = None | Engine | FuelTank

type GameStep = ST.State GameState ()

step : Trigger -> GameStep
step t = ST.returnS ()


