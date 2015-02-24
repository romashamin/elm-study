-- Lets to create figures by mouse dragging

import Text (asText, plainText)
import Signal ((<~), (~), sampleOn, foldp)
import List (map, (::), head, tail)
import Mouse
import Window
import Graphics.Collage (..)
import Graphics.Element (..)
import Color (..)


-- Models

type alias Size = { w:Int, h:Int }

type alias Figure =
  { x1:Int
  , y1:Int
  , x2:Int
  , y2:Int
  , size:Size }

type alias State =
  { isFirstClick:Bool
  , figures:List Figure }


-- Initial state

initState : State
initState =
  { isFirstClick = True
  , figures = [] }


-- Helpers

mapXtoScene x w = toFloat x - toFloat w / 2
mapYtoScene y h = toFloat h / 2 - toFloat y

{-isInside (x, y) =
  let leftX = 0
      rightX = 200
      topY = 0
      bottomY = 200
  in
      (x > leftX && x < rightX) && (y > topY && y < bottomY)-}


-- Logic

input = sampleOn Mouse.isDown Mouse.position

startCreatingFigure (x,y) state =
  let newFigure = { x1 = x
                  , y1 = y
                  , x2 = 0
                  , y2 = 0
                  , size = { w = 0
                           , h = 0 } }
  in
      { state | isFirstClick <- False
              , figures <- newFigure :: state.figures }

updateSize size w h = { size | w <- w, h <- h }

updateFigure (x, y) figure =
  let w = x - figure.x1
      h = y - figure.y1
  in
      { figure | x2 <- x
               , y2 <- y
               , size <- updateSize figure.size w h }

finishCreatingFigure input state =
  { state | isFirstClick <- True
          , figures <- (state.figures |> head |> updateFigure input)
                       :: tail state.figures }

step input state =
  if state.isFirstClick
    then startCreatingFigure input state
    else finishCreatingFigure input state

appState = foldp step initState input


-- View

scene (sceneW, sceneH) appState =
  let drawFigure figure =
        let w = toFloat figure.size.w
            h = toFloat figure.size.h
            x = (mapXtoScene figure.x1 sceneW) + w / 2
            y = (mapYtoScene figure.y1 sceneH) - h / 2
        in
            rect w h |> filled lightGray |> move (x, y)
  in
      layers [ appState |> asText
             , map drawFigure appState.figures |> collage sceneW sceneH ]

main = scene <~ Window.dimensions ~ appState
