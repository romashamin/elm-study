-- In progress...

import Text (asText, plainText)
import Signal (Signal, (<~), (~), foldp)
import Signal
import List ((::))
import List
import Mouse
import Window
import Html
import Html.Attributes (style, for, id, type', value, checked)
import Html.Events (onClick)
import Svg
import Svg.Attributes as SA

--

type ToolName = SelectTool | RectangleTool | CircleTool
type alias Tool = { name:ToolName, active:Bool, icon:Svg.Svg }

type FigureType = Rectangle | Circle
type FigureStatus = Idle | Selected

type alias Figure =
  { id:Int
  , type':FigureType
  , x1:Int, y1:Int
  , x2:Int, y2:Int
  , status:FigureStatus }

type alias State =
  { tools:List Tool
  , figures:List Figure
  , tempFigure:Figure
  , isStartCreating:Bool
  }

type Input = SetActiveTool ToolName
           | MouseClicks (Int,Int)
           | MouseDrags (Int,Int)

svgIcon svgD =
  Svg.svg [ iconStyle ][ Svg.path [ SA.d svgD ][]]

svgSelectIcon = svgIcon "M16.2,14.1l10.2,7.5L23.5,22L22,22.1l0.8,1.3l3.6,6.2l-1.7,1L21,24.4l-0.8-1.3l-0.9,1.2l-1.7,2.3L16.2,14.1 M14.9,11.9 l1.9,17.4l3.3-4.4l4.1,7.1l3.5-2l-4.1-7.1l5.4-0.6L14.9,11.9L14.9,11.9z"
svgRectangleIcon = svgIcon "M31,15v14H13V15H31 M32,14H12v16h20V14L32,14z"
svgCircleIcon = svgIcon "M22,13c5,0,9,4,9,9s-4,9-9,9s-9-4-9-9S17,13,22,13 M22,12c-5.5,0-10,4.5-10,10s4.5,10,10,10s10-4.5,10-10S27.5,12,22,12 L22,12z"

state : State
state =
  { tools =
      [ { name = SelectTool, active = True, icon = svgSelectIcon }
      , { name = RectangleTool, active = False, icon = svgRectangleIcon }
      , { name = CircleTool, active = False, icon = svgCircleIcon }
      ]
  , figures = []
  , tempFigure =
      { id = -1, type' = Rectangle
      , x1 = 0, y1 = 0, x2 = 0, y2 = 0, status = Idle }
  , isStartCreating = True
  }

sizeTreshold = 20
pColor = "#F8E71C"
sColor = "#7FD13B"

--

updateFigX2Y2 fig (x', y') = { fig | x2 <- x', y2 <- y' }

startNewTempFig fig figureType (x', y') =
  { fig | id <- fig.id + 1, type' <- figureType
        , x1 <- x', x2 <- x', y1 <- y', y2 <- y' }

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

startCreatingFigure figureType coords state =
  { state | isStartCreating <- False
          , tempFigure <- startNewTempFig state.tempFigure figureType coords}

update input state =
  let activeTool = List.head <| List.filter (\t -> t.active) state.tools
      figureType =
        case activeTool.name of
          RectangleTool -> Rectangle
          CircleTool -> Circle
          otherwise -> Rectangle
  in
      case input of
        SetActiveTool toolName ->
          let setActiveByName tName t = { t | active <- t.name == tName }
          in
              { state
                  | tools <- List.map (setActiveByName toolName) state.tools }

        MouseClicks coords ->
          if activeTool.name == SelectTool
            then state
            else
              if state.isStartCreating
                then startCreatingFigure figureType coords state
                else finishCreatingFigure coords state

        MouseDrags coords ->
          if activeTool.name == SelectTool
            then state
            else
              { state | tempFigure <- updateFigX2Y2 state.tempFigure coords }

toolsChannel = Signal.channel (SetActiveTool SelectTool)

input : Signal Input
input =
  Signal.mergeMany
    [ Signal.subscribe toolsChannel
    , Signal.map MouseClicks (Signal.sampleOn Mouse.isDown Mouse.position)
    , Signal.map MouseDrags (Signal.keepWhen Mouse.isDown (0,0) Mouse.position)
    ]

currentState = foldp update state input

--

main : Signal Html.Html
main = (view toolsChannel) <~ Window.dimensions ~ currentState

--

view toolsChannel (w,h) state =
  let strW = toString w
      strH = toString h
      strViewBox = "0 0 " ++ strW ++ " " ++ strH
      showMouseDragging =
        if state.isStartCreating
          then drawEmptyFigure
          else drawTempFigure state.tempFigure
  in
      Html.div [ rootStyle ]
        [ Svg.svg
            [ SA.width strW, SA.height strH, SA.viewBox strViewBox ]
            (List.append
              (List.map drawRegularFigure state.figures)
              [ showMouseDragging ])
        , Html.div []
            (List.concat
              [ [ Html.div
                    [ toolbarStyle ]
                    (List.map (toolbarButton toolsChannel) state.tools) ]
              , [ Html.div
                    [ debugTextStyle ]
                    [ Html.text <| (toString state) ] ]
              ])
        ]

drawRegularFigure = drawFigure pColor "0.85"
drawTempFigure = drawFigure sColor "0.5"

drawFigure color opacity' fig =
  case fig.type' of
    Rectangle -> drawRectangle color opacity' fig
    Circle -> drawCircle color opacity' fig

drawRectangle color opacity' fig =
  let strX = toString (if fig.x1 < fig.x2 then fig.x1 else fig.x2)
      strY = toString (if fig.y1 < fig.y2 then fig.y1 else fig.y2)
      strW = toString <| abs <| fig.x1 - fig.x2
      strH = toString <| abs <| fig.y1 - fig.y2
  in
      Svg.rect
        [ SA.fill color, SA.fillOpacity opacity'
        , SA.x strX, SA.y strY, SA.width strW, SA.height strH ][]

drawCircle color opacity' fig =
  let halfW = (fig.x2 - fig.x1) // 2
      halfH = (fig.y2 - fig.y1) // 2
      strCX = toString <| fig.x1 + halfW
      strCY = toString <| fig.y1 + halfH
      strRX = toString <| abs <| halfW
      strRY = toString <| abs <| halfH
  in
      Svg.ellipse
        [ SA.fill color, SA.fillOpacity opacity'
        , SA.cx strCX, SA.cy strCY, SA.rx strRX, SA.ry strRY ][]

drawEmptyFigure =
  Svg.rect
    [ SA.fill "#fff", SA.opacity "0"
    , SA.x "0", SA.y "0", SA.width "0", SA.height "0" ] []

toolbarButton toolsChannel { name, active, icon } =
  Html.button
    [ (toolbarToolStyle active)
    , onClick (Signal.send toolsChannel (SetActiveTool name))
    ]
    [ icon ]

--

rootStyle =
  style
    [ ("-webkit-user-select","none")
    , ("-moz-user-select","none")
    , ("user-select","none")
    , ("cursor","default")
    ]

toolbarStyle =
  style
    [ ("position","fixed")
    , ("top","10px")
    , ("right","10px")
    , ("width","44px")
    , ("font-size","0")
    ]

toolbarToolStyle isActive =
  let bgColor =
        if isActive then "rgba(58,57,58,0.75)" else "rgba(122,121,122,0.75)"
  in
      style
        [ ("margin-bottom","1px")
        , ("padding","0")
        , ("width","44px")
        , ("height","44px")
        , ("border","none")
        , ("background-color", bgColor)
        , ("fill","#EDEDED")
        ]

iconStyle =
  style
    [ ("width","100%")
    , ("height","100%")
    , ("background","inherit")
    , ("fill","inherit")
    , ("transform","translateX(0)")
    , ("-ms-transform","translate(0.5px, -0.3px)")
    ]

debugTextStyle =
  style
    [ ("position","fixed")
    , ("top","10px")
    , ("left","10px")
    , ("right","60px")
    , ("font-family","Inconsolata LGC")
    , ("font-size","8px")
    ]
