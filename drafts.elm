import Text (asText)
import Graphics.Collage (..)
import List (map, (::), head, tail)

data = [ { size={ w=91, h=92 }, id=1 }
       , { size={ w=93, h=94 }, id=2 }
       , { size={ w=95, h=96 }, id=3 } ]

main = map (\item -> (item.size.w, item.size.h)) data |> asText
