-- Playing with signal merging

import Signal (Signal, (<~), (~), map, keepWhen, sampleOn, foldp, merge)
import List ((::))
import Mouse
import Text (asText)

figures = []

drags =
  keepWhen Mouse.isDown (0,0) Mouse.position

clicks =
  foldp (::) figures (sampleOn Mouse.isDown Mouse.position)

view drags clicks =
  asText <| (toString drags) ++ " | " ++ (toString clicks)

main = view <~ drags ~ clicks
