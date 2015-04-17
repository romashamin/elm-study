import Signal
import Signal (Signal, (<~), (~))
import Mouse
import Html
import Html.Events (onClick)

type Input = ToggleStatus | MousePosition (Int,Int)
type alias State = { pos:(Int,Int), status:Bool }

state : State
state = { pos = (0,0), status = False }

update action state =
  case action of
    ToggleStatus -> { state | status <- not state.status }
    MousePosition pos' -> { state | pos <- pos' }

updates = Signal.channel ToggleStatus

{-| It’s impossible to filter signals with state status.
    The solution is to make for every tool its own `update` function:
    https://groups.google.com/d/msg/elm-discuss/rzzDCc7hepY/Emm2KqBOHEIJ
-}
filteredMouse =
  Signal.keepIf (\_ -> state.status) (9,9) Mouse.position

input =
  let filteredMouse =
        if state.status then Mouse.position else Signal.constant (0,0)
  in
      Signal.merge
        (Signal.subscribe updates)
        (Signal.map MousePosition Mouse.position)

stateSignal = Signal.foldp update state input

view state =
  Html.div []
    [ Html.text <| toString state ++ " "
    , Html.button
        [ onClick <| Signal.send updates ToggleStatus ]
        [ Html.text "Toggle" ]
    ]

main : Signal Html.Html
main = view <~ stateSignal
