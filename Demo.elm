
module Demo where

import Dict (empty, values)

import Draw (drawEntity)
import Public.State.State (execState)
import Step (GameState, Mode, None, step)
import Trigger (triggers)
import open Utils

{- Setup initial game state and initialize the loop -}

initialState : GameState
initialState = { entities = empty, mode = initialMode }

initialMode : Mode
initialMode = { pause = False, build = None }

current : Signal GameState
current = foldp (execState . step) initialState triggers

{- Render the display -}

draw : GameState -> [Form]
draw gs = 
  let entities = map (drawEntity 0) . values . .entities <| gs
  in entities

main = combineSElems <|
  [ asText <~ current
  , spaceBlack (400,400) . draw <~ current
  ]
