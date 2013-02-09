
module Main where

import Draw
import Types

main : Element
main = collage 100 100 [drawSpaceCraft $ Brain []]
