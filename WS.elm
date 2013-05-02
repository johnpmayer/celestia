
module WS where

import Graphics.Input
import WebSocket

type TextState = {
  input : String,
  start : Int,
  end   : Int }

(tes, msgs) = text "message" {input="Hi! I am John",start=0,end=1}
(be, send) = button "send"

transmit = sampleOn send (lift (\x -> x.input) msgs)

receive = open "ws://192.168.2.3:9160/" transmit

display te me = flow down [te,be,me]

messages = lift plainText receive

main = display <~ tes ~ messages

