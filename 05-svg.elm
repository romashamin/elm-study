-- Lets to create SVG figures by mouse dragging

import Svg (..)
import Svg.Attributes (..)
import Html (Html)
import Window
import Signal (Signal, (<~), (~), sampleOn, foldp)
import List (map, (::), head, tail)
import Mouse

--

type alias Figure =
  { x:Int
  , y:Int
  , w:Int
  , h:Int
  }

type alias State =
  { isFirstClick:Bool
  , figures:List Figure
  }

--

initialState : State
initialState =
  { isFirstClick = True
  , figures = []
  }

color = "#7FD13B"

--

updateFigure (x', y') figure =
  let w' = x' - figure.x
      h' = y' - figure.y
  in
      { figure | w <- w', h <- h' }

finishCreatingFigure input state =
  { state | isFirstClick <- True
          , figures <- (state.figures |> head |> updateFigure input)
                       :: tail state.figures }

startCreatingFigure (x',y') state =
  let newFigure = { x = x', y = y', w = 0, h = 0 }
  in
      { state | isFirstClick <- False
              , figures <- newFigure :: state.figures }

update input state =
  if state.isFirstClick
    then startCreatingFigure input state
    else finishCreatingFigure input state

input = sampleOn Mouse.isDown Mouse.position

model = foldp update initialState input

--

view (winW, winH) model =
  let strW = toString winW
      strH = toString winH
      strViewBox = "0 0 " ++ strW ++ " " ++ strH
      drawFigure figure =
        let strX = toString figure.x
            strY = toString figure.y
            strW = toString figure.w
            strH = toString figure.h
        in
            rect [ fill color, x strX, y strY, width strW, height strH ] []
  in
      svg
        [ version "1.1", width strW, height strH, viewBox strViewBox ]
        (map drawFigure model.figures)

--

main : Signal Html
main = view <~ Window.dimensions ~ model
