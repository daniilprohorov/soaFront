module Pages.MainPage exposing (..)


import Element exposing (paragraph,text)
import Element.Font exposing (center, size)
import Element.Input exposing (button)
import Maybe exposing (withDefault)
import Pages.Layouts exposing (centerLayout)
import Pages.Styles exposing (buttonStyle)
import Types exposing (Msg(..), ToPage(..))


showMainPage s a =
    centerLayout
        [ paragraph [center, size 50] [ text "Main page" ]
        , button buttonStyle {onPress=Just <| Go ToVehiclesPage, label=text "vehicles page"}
        , paragraph [center, size 50] [ text "Price sum: ", text <| withDefault "ERROR SUM" s ]
        , paragraph [center, size 50] [ text "Price avg: ", text <| withDefault "ERROR AVG" a ]
        ]
