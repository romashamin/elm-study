-- Playing with signal filtering

import Signal (Signal, (<~), (~), keepWhen)
import Mouse
import Text (asText)

drags =
  keepWhen Mouse.isDown (0,0) Mouse.position

view drags =
  asText <| drags

main = view <~ drags
