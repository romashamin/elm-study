-- Playing with keyboard

import Signal (Signal, (<~), (~), foldp, sampleOn)
import Signal
import Keyboard
import Time (..)
import Text (asText)

defV = 1
supV = 50

state = { x = 500, y = 500, v = defV }

update ({x,y}, isShiftPressed) state =
  { state | v <- if isShiftPressed then supV else defV
          , x <- state.x + (x * state.v)
          , y <- state.y + (y * state.v) }

input =
  sampleOn delta ((,) <~ Keyboard.arrows ~ Keyboard.shift)

delta = Signal.map (\t -> t / 20) (fps 15)

stateSignal = foldp update state input

main = asText <~ stateSignal
