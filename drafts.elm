import Text (asText)
import List (filter, head)

figures =
  [ { id = 1, x = 100, y = 100, isSelected = True }
  , { id = 2, x = 300, y = 100, isSelected = False }
  , { id = 3, x = 500, y = 100, isSelected = False }
  ]

figureWithId id =
  filter (\f -> f.id == id) figures

main = asText <| head <| figureWithId 2
