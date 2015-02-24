import Text (asText)
import Graphics.Collage (..)
import List (map)

coords = [ (40, 50), (140, 150) ]

drawCircle (x, y) =
  (round x, round y)
    |> asText
    |> toForm
    |> move (x, y)

double (x, y) = (x * 2, y * 2)

main = map drawCircle coords |> collage 800 600
