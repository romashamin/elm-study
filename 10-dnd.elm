-- Basic drag and drop

import Text (asText, plainText)
import Signal (Signal, (<~), (~), foldp)
import Signal
import List
import Mouse
import Window
import Html.Events (onClick, onMouseDown, onMouseUp)
import Svg
import Svg.Attributes (..)
import Html

--

type Status = Idle | Selected
type Input = NoOp | Select Int | ClearSelections | MouseDrags (Int,Int)
type alias Figure = { id:Int, x1:Int, y1:Int, x2:Int, y2:Int, status:Status }

state =
  { figures =
    [ { id=1, x1=100, y1=100, x2=200, y2=160, status=Idle }
    , { id=2, x1=300, y1=100, x2=500, y2=200, status=Idle }
    , { id=3, x1=600, y1=100, x2=700, y2=400, status=Idle }
    ]
  }

pColor = "#F8E71C"
sColor = "#7FD13B"
sSColor = "#037BFF"

--

moveDraggableFigure (x,y) figure =
  let w = figure.x2 - figure.x1
      h = figure.y2 - figure.y1
  in
      if figure.status == Selected
        then { figure | x1 <- x
                      , x2 <- x + w
                      , y1 <- y
                      , y2 <- y + h }
        else figure

update input state =
  let unselectedFigures =
        List.map (\f -> { f | status <- Idle } ) state.figures
  in
      case input of
        NoOp -> state

        Select id ->
          let toggleById figure =
                if figure.id == id
                  then { figure | status <- Selected } else figure
          in
              { state | figures <- List.map toggleById unselectedFigures }

        ClearSelections ->
          { state | figures <- unselectedFigures }

        MouseDrags coords ->
          { state | figures <- List.map (moveDraggableFigure coords) state.figures }

updates = Signal.channel NoOp

input : Signal Input
input =
  Signal.merge
    (Signal.subscribe updates)
    (Signal.map MouseDrags (Signal.keepWhen Mouse.isDown (0,0) Mouse.position))

currentState = foldp update state input

--

drawFigure updates color opacity' fig =
  let strX = toString (if fig.x1 < fig.x2 then fig.x1 else fig.x2)
      strY = toString (if fig.y1 < fig.y2 then fig.y1 else fig.y2)
      strW = toString <| abs <| fig.x1 - fig.x2
      strH = toString <| abs <| fig.y1 - fig.y2
      strokePart =
        if fig.status == Selected
          then [stroke sSColor, strokeWidth "2"]
          else []
  in
      Svg.rect
        (List.append
          strokePart
          [ fill color, opacity opacity', x strX, y strY, width strW, height strH
          , onMouseDown (Signal.send updates (Select fig.id))])
        []

drawIdleFigure updates = drawFigure updates pColor "1"
drawSelectedFigure updates = drawFigure updates pColor "0.5"

placeFigure updates figure =
  if figure.status == Selected
    then drawSelectedFigure updates figure
    else drawIdleFigure updates figure

view updates (w,h) state =
  let strW = toString w
      strH = toString h
      strViewBox = "0 0 " ++ strW ++ " " ++ strH
  in
      Svg.svg
        [ version "1.1", width strW, height strH, viewBox strViewBox
        , onMouseUp (Signal.send updates ClearSelections) ]
        (List.append
          (List.map (placeFigure updates) state.figures)
          [ Svg.text
              [ x "0", y "8"
              , fontFamily "Inconsolata LGC", fontSize "8" ]
              [ Html.text <| (toString state) ] ]
          )

--

main : Signal Html.Html
main = (view updates) <~ Window.dimensions ~ currentState
