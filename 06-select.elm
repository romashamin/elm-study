{-  Lets select any figure by click
    or clear a selection by click on canvas
-}

import Signal (Signal, (<~), (~), foldp, subscribe, channel, send)
import Mouse
import Window
import List (map, filter, append)
import Svg
import Svg.Attributes (..)
import Text (asText)
import Html (Html, div, button, text)
import Html.Attributes (style)
import Html.Events (onClick)


type alias Figure =
  { id:Int, x:Int, y:Int, isSelected:Bool }

state =
  { figures =
      [ { id = 1, x = 100, y = 100, isSelected = False }
      , { id = 2, x = 300, y = 100, isSelected = False }
      , { id = 3, x = 500, y = 100, isSelected = False }
      ]
  }

defWidth = "180"
defHeight = "60"
fillColor = "#F8E71C"
strokeColor = "#037BFF"

--

type Action = NoOp | Select Int | ClearSelections

update action state =
  let unselectedFigures =
        map (\f -> { f | isSelected <- False } ) state.figures
  in
      case action of
        NoOp -> state

        Select id ->
          let selectById f =
                if f.id == id then { f | isSelected <- True } else f
          in
              { state | figures <- map selectById unselectedFigures }

        ClearSelections ->
          { state | figures <- unselectedFigures }

updates = channel NoOp

model = foldp update state (subscribe updates)

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
          , onClick (send updates (Select figure.id)) ])
        []

view updates (w, h) model =
  let sw = toString w
      sh = toString h
      svb = "0 0 " ++ sw ++ " " ++ sh
  in
      map drawFigure model.figures
        |> Svg.svg
              [ version "1.1", width sw, height sh, viewBox svb
              , onClick (send updates ClearSelections) ]

main : Signal Html
main = (view updates) <~ Window.dimensions ~ model
