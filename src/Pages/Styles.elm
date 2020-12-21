module Pages.Styles exposing (..)

import Element exposing (centerX, padding, rgb255)
import Element.Border as Border

buttonStyle =
    [ padding 5
    , Border.width 1
    , Border.rounded 3
    , Border.color <| rgb255 200 200 200
    , centerX
    ]

