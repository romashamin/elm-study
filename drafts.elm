import Signal
import Signal (Signal, (<~), (~))
import Mouse
import Keyboard
import Time
import Html
import Html.Events (onClick)
import Text (asText)

type Action = NoOp | Increase | Decrease | KeyboardArrows { x:Int, y:Int }

state = { value = 0 }

update action state =
  case action of
    NoOp -> state
    Increase -> { state | value <- state.value + 1 }
    Decrease -> { state | value <- state.value - 1 }
    KeyboardArrows {x,y} -> { state | value <- state.value + y + x }

updates = Signal.channel NoOp

delta = Signal.map (\t -> t / 20) (Time.fps 15)

keyboardArrows = Signal.sampleOn delta Keyboard.arrows

input =
  Signal.merge
    (Signal.subscribe updates)
    (Signal.map KeyboardArrows keyboardArrows)

stateSignal = Signal.foldp update state input

view updates state =
  Html.div []
    [ Html.button [ onClick <| Signal.send updates Decrease ] [ Html.text "–" ]
    , Html.text <| " " ++ toString state.value ++ " "
    , Html.button [ onClick <| Signal.send updates Increase ] [ Html.text "+" ]
    ]

main : Signal Html.Html
main = (view updates) <~ stateSignal
