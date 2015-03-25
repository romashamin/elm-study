-- In progress...

import Text (asText, plainText)
import Signal (Signal, (<~), (~), map, keepWhen, sampleOn, foldp, merge)
import Mouse
import Window
import Svg
import Svg.Attributes (..)
import Html

--

type alias Figure = { id:Int, x:Int, y:Int, w:Int, h:Int, isSelected:Bool }

type Update = MouseClicks (Int,Int) | MouseDrags (Int,Int)

state =
  { isStartCreating = True
  , figures = []
  , tempFigure = { id = -1, x = 0, y = 0, w = 0, h = 0, isSelected = False }
  , currentPos = { x = 0, y = 0 }
  }

--

updateWH fig (x', y') =
  { fig | w <- abs <| x' - fig.x
        , h <- abs <| y' - fig.y }

finishCreatingFigure coords state =
  { state | isStartCreating <- True
          , tempFigure <- updateWH state.tempFigure coords }

updateXY fig (x', y') =
  { fig | x <- x', y <- y' }

updateCurrentPos pos (x', y') =
  { pos | x <- x', y <- y' }

startCreatingFigure coords state =
  { state | isStartCreating <- False
          , tempFigure <- updateXY state.tempFigure coords
          , currentPos <- updateCurrentPos state.currentPos coords }

update input state =
  case input of
    MouseClicks coords ->
      if state.isStartCreating
        then startCreatingFigure coords state
        else finishCreatingFigure coords state

    MouseDrags coords ->
      { state | currentPos <- updateCurrentPos state.currentPos coords }

currentState =
  foldp update state updates

updates : Signal Update
updates =
  merge
    (map MouseClicks (sampleOn Mouse.isDown Mouse.position))
    (map MouseDrags (keepWhen Mouse.isDown (0,0) Mouse.position))

--

view (w,h) state =
  let strW = toString w
      strH = toString h
      strViewBox = "0 0 " ++ strW ++ " " ++ strH
      strTmpX = toString state.tempFigure.x
      strTmpY = toString state.tempFigure.y
      strTmpW = toString <| abs <| state.currentPos.x - state.tempFigure.x
      strTmpH = toString <| abs <| state.currentPos.y - state.tempFigure.y
      s = " (" ++ strTmpX ++ "," ++ strTmpY ++ "," ++ strTmpW ++ "," ++ strTmpH ++ ")"
  in
    Svg.svg
      [ version "1.1", width strW, height strH, viewBox strViewBox ]
      [ Svg.rect
          [ fill "#F8E71C", x strTmpX, y strTmpY, width strTmpW, height strTmpH ]
          []
      , Svg.text
          [ x "0", y "8"
          , fontFamily "Inconsolata LGC", fontSize "8" ]
          [ Html.text <| (toString state) ++ s ] ]

--

main : Signal Html.Html
main =
  view <~ Window.dimensions ~ currentState
