-- In progress...

import Text (asText, plainText)
import Signal (Signal, (<~), (~), map, keepWhen, sampleOn, foldp, merge)
import List ((::), append)
import List
import Mouse
import Window
import Svg
import Svg.Attributes (..)
import Html

--

type alias Figure = { id:Int, x1:Int, y1:Int, x2:Int, y2:Int, isSelected:Bool }

type Update = MouseClicks (Int,Int) | MouseDrags (Int,Int)

state =
  { isStartCreating = True
  , figures = []
  , tempFigure = { id = -1, x1 = 0, y1 = 0, x2 = 0, y2 = 0, isSelected = False }
  }

sizeTreshold = 20
pColor = "#F8E71C"
sColor = "#7FD13B"

--

updateFigX2Y2 fig (x', y') = { fig | x2 <- x', y2 <- y' }

startNewTempFig fig (x', y') =
  { fig | id <- fig.id + 1 , x1 <- x', x2 <- x', y1 <- y', y2 <- y' }

finishCreatingFigure coords state =
  let isFigureTooSmall fig =
        (abs (fig.x1 - fig.x2) <= sizeTreshold) &&
        (abs (fig.y1 - fig.y2) <= sizeTreshold)
  in
      { state | isStartCreating <- True
              , tempFigure <- updateFigX2Y2 state.tempFigure coords
              , figures <-
                  if isFigureTooSmall state.tempFigure
                    then state.figures
                    else state.tempFigure :: state.figures }

startCreatingFigure coords state =
  { state | isStartCreating <- False
          , tempFigure <- startNewTempFig state.tempFigure coords}

update input state =
  case input of
    MouseClicks coords ->
      if state.isStartCreating
        then startCreatingFigure coords state
        else finishCreatingFigure coords state

    MouseDrags coords ->
      { state | tempFigure <- updateFigX2Y2 state.tempFigure coords }

currentState = foldp update state updates

updates : Signal Update
updates =
  merge
    (map MouseClicks (sampleOn Mouse.isDown Mouse.position))
    (map MouseDrags (keepWhen Mouse.isDown (0,0) Mouse.position))

--

drawFigure color opacity' fig =
  let strX = toString (if fig.x1 < fig.x2 then fig.x1 else fig.x2)
      strY = toString (if fig.y1 < fig.y2 then fig.y1 else fig.y2)
      strW = toString <| abs <| fig.x1 - fig.x2
      strH = toString <| abs <| fig.y1 - fig.y2
  in
      Svg.rect
        [ fill color, opacity opacity', x strX, y strY, width strW, height strH ]
        []

drawRegularFigure = drawFigure pColor "0.85"
drawTempFigure = drawFigure sColor "0.5"

view (w,h) state =
  let strW = toString w
      strH = toString h
      strViewBox = "0 0 " ++ strW ++ " " ++ strH
      showMouseDragging =
        if state.isStartCreating
          then Svg.rect [ fill "#fff", x "0", y "0", width "0", height "0" ] []
          else drawTempFigure state.tempFigure
  in
      Svg.svg
        [ version "1.1", width strW, height strH, viewBox strViewBox ]
        (append
          (List.map drawRegularFigure state.figures)
          [ showMouseDragging
          , Svg.text
              [ x "0", y "8"
              , fontFamily "Inconsolata LGC", fontSize "8" ]
              [ Html.text <| (toString state) ] ]
          )

--

main : Signal Html.Html
main = view <~ Window.dimensions ~ currentState
