
-- State Monad

module State where

type State s a = { runState : s -> (a, s) }

get : State s s
get = State <| \s -> (s, s)

put : s -> State s ()
put s' = State <| \s -> ((), s')

evalState : State s a -> s -> a
evalState st = fst . st.runState

execState : State s a -> s -> s
execState st = snd . st.runState

{- Monad Instance -}

returnS : a -> State s a
returnS x = State <| \s -> (x, s)

bindS : State s a -> (a -> State s b) -> State s b
bindS st f = 
  State <| 
    \s -> let (x, s') = st.runState s
          in (f x).runState s'

sequenceS : [State s a] -> State s [a]
sequenceS ms = 
  let k m m' = 
    bindS m (\x ->
    bindS m' (\xs ->
    returnS <| x :: xs))
  in foldr k (returnS []) ms

mapMS : (a -> State s b) -> [a] -> State s [b]
mapMS f = sequenceS . map f

{- Functor Instance (derived) -}

fmapS : (a -> b) -> State s a -> State s b
fmapS f st =
  bindS st (\x ->
  returnS <| f x)

