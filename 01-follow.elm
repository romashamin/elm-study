-- Makes a circle to follow the mouse

import Text (asText)
import Mouse
import Signal ((<~), (~))
import Window
import Graphics.Collage (..)
import Color (..)
import Graphics.Element (..)

size = 40

transx x w = toFloat x - toFloat w / 2
transy y h = toFloat h / 2 - toFloat y

fig = circle size |> filled lightGray
coords x y =
  (round x, round y) |> asText |> toForm |> moveY (-size)

scene (w, h) (x, y) =
  let fx = transx x w
      fy = transy y h
  in
      collage w h [
        group [ fig, coords fx fy ] |> move (fx, fy)
      ]

main = scene <~ Window.dimensions ~ Mouse.position
