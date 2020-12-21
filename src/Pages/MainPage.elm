module Pages.MainPage exposing (..)


import Element exposing (paragraph,text)
import Element.Font exposing (center, size)
import Element.Input exposing (button)
import Pages.Layouts exposing (centerLayout)
import Pages.Styles exposing (buttonStyle)
import Types exposing (Msg(..), ToPage(..))

showMainPage =
    centerLayout
        [ paragraph [center, size 50] [ text "Main page" ]
        , button buttonStyle {onPress=Just <| Go ToProductsPage, label=text "Products page"}
        , button buttonStyle {onPress=Just <| Go ToOrganizationsPage, label=text "Organizations page"}
        ]
