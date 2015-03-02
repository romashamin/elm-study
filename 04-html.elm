-- Lets change a name in the greeting

import Html (..)
import Html.Attributes (style, for, id, name, type', value)
import Html.Events (on, targetValue)
import Signal ((<~), (~), sampleOn, foldp, subscribe, channel, send)
import List (map, (::), head, tail)
import Mouse
import String

initState =
  { greeting = "Hello, "
  , name = "Ko-ko-ko"
  , mark = "!"
  }

type Action = NoOp | UpdateName String

update action model =
  case action of
    NoOp -> model
    UpdateName newName -> { model | name <- newName }

updates = channel NoOp

view updates model =
  div [ style [ ("width","300px")
              , ("margin","5% auto")
              , ("padding","10px 20px")
              , ("color","white")
              , ("background-color","#2ECC71")
              ]
      ]
    [ p [ style [ ("font-weight","bold") ] ]
        [ text <| model.greeting ++ model.name ++ model.mark ]
    , form []
        [ p []
            [ label [ for "name" ]
                [ span [] [ text "Name " ]
                , input
                    [ id "name"
                    , name "name"
                    , type' "text"
                    , value model.name
                    , style [ ("font-size","inherit") ]
                    , on "input" targetValue (send updates << UpdateName)
                    ]
                    []
                ]
            ]
        ]
    ]

model = foldp update initState (subscribe updates)

main = (view updates) <~ model
