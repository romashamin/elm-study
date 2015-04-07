{-  Lets select any figure by click
    or clear a selection by click on canvas
-}

import Signal (Signal, (<~), (~), foldp)
import Signal
import Mouse
import Window
import List (map, filter, append)
import Svg
import Svg.Attributes (..)
import Text (asText)
import Html
import Html (Html, div, button, text)
import Html.Attributes (style)
import Html.Events (onClick)
import Keyboard
import Time


type alias Figure =
  { id:Int, x:Int, y:Int, isSelected:Bool }

state =
  { figures =
      [ { id = 1, x = 100, y = 100, isSelected = False }
      , { id = 2, x = 300, y = 100, isSelected = False }
      , { id = 3, x = 500, y = 100, isSelected = False }
      , { id = 4, x = 700, y = 100, isSelected = False }
      ]
  , isShiftPressed = False
  }

defWidth = "180"
defHeight = "60"
fillColor = "#F8E71C"
strokeColor = "#037BFF"

--

type Action = NoOp | Select Int | ClearSelections | KeyboardShift Bool

update action state =
  let unselectedFigures =
        map (\f -> { f | isSelected <- False } ) state.figures
  in
      case action of
        NoOp -> state

        Select id ->
          let toggleSelection cond = not cond
              toggleById f =
                if f.id == id
                  then { f | isSelected <- toggleSelection f.isSelected } else f
              figures' =
                if state.isShiftPressed
                  then state.figures else unselectedFigures
          in
              { state | figures <- map toggleById figures' }

        ClearSelections ->
          { state | figures <- unselectedFigures }

        KeyboardShift isShiftPressed' ->
          { state | isShiftPressed <- isShiftPressed' }

updates = Signal.channel NoOp

delta = Signal.map (\t -> t / 20) (Time.fps 15)

keyboardShift = Signal.sampleOn delta Keyboard.shift

input =
  Signal.merge
    (Signal.subscribe updates)
    (Signal.map KeyboardShift keyboardShift)

model = foldp update state input

--

drawFigure figure =
  let x' = toString figure.x
      y' = toString figure.y
      strokePart =
        if figure.isSelected
          then [stroke strokeColor, strokeWidth "2"]
          else []
  in
      Svg.rect
        (append
          strokePart
          [ fill fillColor, x x', y y'
          , width defWidth, height defHeight
          , onClick (Signal.send updates (Select figure.id)) ])
        []

view updates (w, h) model =
  let sw = toString w
      sh = toString h
      svb = "0 0 " ++ sw ++ " " ++ sh
  in
      append
        (map drawFigure model.figures)
        [ Svg.text
          [ x "0", y "8"
          , fontFamily "Inconsolata LGC", fontSize "8" ]
          [ Html.text <| (toString model) ] ]
        |> Svg.svg
            [ version "1.1", width sw, height sh, viewBox svb
            , onClick (Signal.send updates ClearSelections) ]

main : Signal Html
main = (view updates) <~ Window.dimensions ~ model
