
module Keys where

import open Char
import Keyboard as K

main = (\ses -> flow down <~ combine ses) <|
  [ asText <~ constant "A bunch of key signals:"
  , asText <~ K.keysDown
  , asText <~ K.lastPressed
  , asText <~ K.space
  , asText <~ (K.isDown <| toCode 'x')
  , asText <~ K.wasd
  , asText <~ (constant <| toCode 'x')
  ]

