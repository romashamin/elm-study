-- Click creates a circle

import Text (asText, plainText)
import List (map, (::))
import Mouse
import Signal ((<~), (~), sampleOn, foldp)
import Graphics.Collage (..)
import Color (..)
import Graphics.Element (..)
import Window

circleSize = 60

mapXtoW x w = toFloat x - toFloat w / 2
mapYtoH y h = toFloat h / 2 - toFloat y

clickCoords =
  foldp (::) [] (sampleOn Mouse.clicks Mouse.position)

scene (sceneW, sceneH) coords =
  let drawCircle (x', y') =
        let x = mapXtoW x' sceneW
            y = mapYtoH y' sceneH
        in
            group [ circle circleSize |> filled lightGray |> move (x, y)
                  , (x, y) |> asText |> toForm |> move (x, y)
                  ]
  in
      layers [ plainText "Click to create circle"
             , map drawCircle coords |> collage sceneW sceneH
             ]

main = scene <~ Window.dimensions ~ clickCoords
