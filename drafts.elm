import Signal (Signal, (<~), (~), map, keepWhen, sampleOn, foldp, merge)
import List ((::))
import Mouse
import Text (asText)
import Time (fps)

type Update = MouseClicks (Int,Int) | MouseDrags (Int,Int)

state =
  { figures = []
  , currentPos = { x = 0, y = 0 }
  }

inputs : Signal Update
inputs =
  merge
    (map MouseClicks (sampleOn Mouse.isDown Mouse.position))
    (map MouseDrags (keepWhen Mouse.isDown (0,0) Mouse.position))

update input state =
  case input of
    MouseClicks coords ->
      { state | figures <- coords :: state.figures }

    MouseDrags (x,y) ->
      let updateCurrentPos pos x' y' =
            { pos | x <- x', y <- y' }
      in
          { state | currentPos <- updateCurrentPos state.currentPos x y }


currentState =
  foldp update state inputs

main = asText <~ currentState
