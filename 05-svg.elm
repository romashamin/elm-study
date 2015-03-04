-- Lets to create SVG figures by mouse dragging

import Svg (..)
import Svg.Attributes (..)
import Html
import Window
import Signal (Signal, (<~), (~), sampleOn, foldp)
import List (map, (::), head, tail, append)
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

updateFigure (x2, y2) figure =
  let x' = if x2 < figure.x then x2 else figure.x
      y' = if y2 < figure.y then y2 else figure.y
      w' = abs <| x2 - figure.x
      h' = abs <| y2 - figure.y
  in
      { figure | x <- x', y <- y', w <- w', h <- h' }

finishCreatingFigure (x2, y2) state =
  let sizeTreshold = 5
      currentFigure = state.figures |> head
      isFigureTooSmall =
        (abs (x2 - currentFigure.x) <= sizeTreshold) &&
        (abs (y2 - currentFigure.y) <= sizeTreshold)
  in
      { state | isFirstClick <- True
              , figures <- if isFigureTooSmall
                    then tail state.figures
                    else (currentFigure |> updateFigure (x2, y2))
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

view (winW, winH) (mouseX, mouseY) model =
  let strMX = toString mouseX
      strMY = toString mouseY
      strW = toString winW
      strH = toString winH
      strViewBox = "0 0 " ++ strW ++ " " ++ strH
      drawFigure figure =
        let strX = toString figure.x
            strY = toString figure.y
            strW = toString figure.w
            strH = toString figure.h
        in
            rect
              [ fill color, x strX, y strY
              , width strW, height strH ]
              []
  in
      svg
        [ version "1.1", width strW, height strH, viewBox strViewBox ]
        (append
          (map drawFigure model.figures)
          [ line
              [ x1 strMX, y1 "0", x2 strMX, y2 strH
              , stroke "#f0f", strokeWidth "0.05" ]
              []
          , line
              [ x1 "0", y1 strMY, x2 strW, y2 strMY
              , stroke "#f0f", strokeWidth "0.05" ]
              []
          , text
              [ x "0", y "8"
              , fontFamily "Inconsolata LGC", fontSize "8" ]
              [ Html.text <| toString <| model.figures ]
          ]
        )

--

main : Signal Html.Html
main = view <~ Window.dimensions ~ Mouse.position ~ model
