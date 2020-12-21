module Pages.ProductsPage exposing (..)

import DataField exposing (DataField(..))
import Element exposing (centerX, paragraph, text)
import Element.Font exposing (center)
import Element.Input as Input exposing (button)
import Pages.Layouts exposing (centerLayout, centerXLayout)
import Pages.Styles exposing (buttonStyle)
import Products exposing (printProducts)
import Types exposing (Msg(..), Operation(..), ToPage(..))

showProductsPage operation data = case operation of
    Show -> showProducts data
    Add -> addProduct data
    _ -> Debug.todo "lul"

showProducts data = case data of
    Just (Prds products) -> centerXLayout
        [ paragraph [ center ] [ text "Products page" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , paragraph [centerX] [ printProducts products ]
        , Input.text []
            { onChange=(\s -> Go ToMainPage)
            , text = "Name"
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "Label")
            }
        ]

    Just _ -> Debug.todo "show products error"

    Nothing -> centerXLayout
        [ paragraph [ center ] [ text "Products page" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , paragraph [centerX] [ text "wait" ]
        ]

addProduct data = case data of
    Just (Prd product) -> centerXLayout
        [ paragraph [ center ] [ text "Products page" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        ]
    Just _ -> Debug.todo "add products error"

    Nothing -> centerXLayout
        [ paragraph [ center ] [ text "Products page" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , paragraph [centerX] [ text "wait" ]
        ]
