-- Lets you choose a tool

import Signal (Signal, (<~), (~), foldp)
import Signal
import List ((::))
import List
import Mouse
import Html
import Html.Attributes (style, for, id, type', value, checked)
import Html.Events (onClick)
import Svg
import Svg.Attributes as SA

type ToolName = SelectTool | RectangleTool | CircleTool
type alias Tool = { name:ToolName, active:Bool, icon:Svg.Svg }

type FigureType = Rectangle | Circle
type alias Figure = { type':FigureType, pos:(Int,Int) }

type Input = SetActive ToolName | MouseClicks (Int,Int)

svgIcon svgD =
  Svg.svg [ iconStyle ][ Svg.path [ SA.d svgD ][]]

svgSelectIcon = svgIcon "M16.2,14.1l10.2,7.5L23.5,22L22,22.1l0.8,1.3l3.6,6.2l-1.7,1L21,24.4l-0.8-1.3l-0.9,1.2l-1.7,2.3L16.2,14.1 M14.9,11.9 l1.9,17.4l3.3-4.4l4.1,7.1l3.5-2l-4.1-7.1l5.4-0.6L14.9,11.9L14.9,11.9z"
svgRectangleIcon = svgIcon "M31,15v14H13V15H31 M32,14H12v16h20V14L32,14z"
svgCircleIcon = svgIcon "M22,13c5,0,9,4,9,9s-4,9-9,9s-9-4-9-9S17,13,22,13 M22,12c-5.5,0-10,4.5-10,10s4.5,10,10,10s10-4.5,10-10S27.5,12,22,12 L22,12z"

state =
  { tools =
      [ { name = SelectTool, active = True, icon = svgSelectIcon }
      , { name = RectangleTool, active = False, icon = svgRectangleIcon }
      , { name = CircleTool, active = False, icon = svgCircleIcon }
      ]
  , figures = []
  }

update input state =
  case input of
    SetActive toolName ->
      let setActiveByName tName t =
            { t | active <- if t.name == tName then True else False }
      in
          { state | tools <- List.map (setActiveByName toolName) state.tools }

    MouseClicks coords ->
      let activeTool = List.head <| List.filter (\t -> t.active) state.tools
          addNewFigure figureType =
              let newFigure = { type' = figureType, pos = coords }
              in
                  { state | figures <- newFigure :: state.figures }
      in
          case activeTool.name of
            SelectTool -> state
            RectangleTool -> addNewFigure Rectangle
            CircleTool -> addNewFigure Circle

toolsChannel = Signal.channel (SetActive SelectTool)

input : Signal Input
input =
  Signal.merge
    (Signal.subscribe toolsChannel)
    (Signal.map MouseClicks (Signal.sampleOn Mouse.clicks Mouse.position))

currentState = foldp update state input

--

main : Signal Html.Html
main = (view toolsChannel) <~ currentState

--

view toolsChannel state =
  Html.div []
    (List.concat
      [ [ Html.div [ toolbarStyle ]
            (List.map (toolbarButton toolsChannel) state.tools) ]
      , [ Html.div [][ Html.text <| (toString state) ] ]
      ])

toolbarButton toolsChannel { name, active, icon } =
  Html.button
    [ (toolbarToolStyle active)
    , onClick (Signal.send toolsChannel (SetActive name))
    ]
    [ icon ]

--

toolbarStyle =
  style
    [ ("position","fixed")
    , ("top","10px")
    , ("right","10px")
    , ("width","44px")
    , ("font-size","0")
    ]

toolbarToolStyle isActive =
  let bgColor = if isActive then "#3A393A" else "#7A797A"
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
