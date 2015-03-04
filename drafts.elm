import Text (asText)
import Html
import Svg (..)
import Svg.Attributes (..)


main : Html.Html
main =
  svg
    [ version "1.1", width "800", height "600", viewBox "0 0 800 600" ]
    [ rect [ fill "black", x "100", y "100", width "40", height "40" ] []
    , text [ x "100", y "160" ] [ Html.text "Hello!" ]
    ]
