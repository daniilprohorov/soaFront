module Pages.ProductsPage exposing (..)

import DataField exposing (DataField(..), DataFieldInput(..))
import Element exposing (centerX, paragraph, text)
import Element.Font exposing (center)
import Element.Input as Input exposing (button)
import Pages.Elements exposing (inputProducts)
import Pages.Layouts exposing (centerLayout, centerXLayout)
import Pages.Styles exposing (buttonStyle)
import Products exposing (printProducts, productInputDef, Product)
import Types exposing (ActionType(..), Msg(..), Operation(..), ToPage(..))

showProductsPage operation data = case operation of
    Show -> showProducts data
    Add -> addProduct Start
    _ -> Debug.todo "lul"

showProducts data = case data of
    Just (Prds products) -> centerXLayout
        [ paragraph [ center ] [ text "Products page" ]
        , button buttonStyle {onPress=Just <| PageAction Add Start, label=text "Add Product"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        --, Input.text []
        --    { onChange=(\s -> Go ToMainPage)
        --    , text = "Name"
        --    , placeholder = Nothing
        --    , label = Input.labelAbove [] (text "Label")
        --    }
        , paragraph [centerX] [ printProducts products ]
        ]

    Just _ -> Debug.todo "show products error"

    Nothing -> centerXLayout
        [ paragraph [ center ] [ text "Products page" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , paragraph [centerX] [ text "wait" ]
        ]

addProduct data = case data of
    Start -> centerXLayout
        [ paragraph [ center ] [ text "Add Products" ]
        --, button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Validate and send"}
        , paragraph [centerX] [ text "wait" ]
        , inputProducts (PrdInp productInputDef)
        ]

    Store dataFieldInput -> centerXLayout
        [ paragraph [ center ] [ text "Add Products" ]
        , button buttonStyle {onPress=Just <| PageAction Add (Check dataFieldInput), label=text "Validate and send"}
        , paragraph [centerX] [ text "wait" ]
        , inputProducts dataFieldInput
        ]

    Fail string -> Debug.todo "fail"

    Send dataField -> Debug.todo "Send"

    Check dataFieldInput -> Debug.todo "check"

