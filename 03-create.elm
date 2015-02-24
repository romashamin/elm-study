-- In progress...

import Text (asText, plainText)
import Signal ((<~), (~), sampleOn, foldp)
import Mouse
import Window
import Graphics.Collage (..)
import Color (..)

clickCoords =
  foldp (::) [] (sampleOn Mouse.isDown Mouse.position)

isInside (x, y) =
  let leftX = 0
      rightX = 200
      topY = 0
      bottomY = 200
  in
      (x > leftX && x < rightX) && (y > topY && y < bottomY)

scene (x, y) drag =
  asText <| if drag then (x, y) else (0, 0)

main = scene <~ Mouse.position ~ Mouse.isDown
