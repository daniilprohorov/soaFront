module Pages.ProductsPage exposing (..)

import DataField exposing (DataField(..), DataFieldInput(..))
import Element exposing (centerX, paragraph, text)
import Element.Font exposing (center)
import Element.Input as Input exposing (button)
import Pages.Elements exposing (inputProducts)
import Pages.Layouts exposing (centerLayout, centerXLayout)
import Pages.Styles exposing (buttonStyle)
import Products exposing (printProducts, productInputDef, Product)
import Types exposing (ActionType(..), Msg(..), Operation(..), ToPage(..), addProductMsgDef)

showProductsPage operation data = case operation of
    Show -> showProducts data
    Add (PrdInp dataFieldInput) send fail -> addProduct dataFieldInput send fail
    _ -> Debug.todo "lul"

showProducts data = case data of
    Just (Prds products) -> centerXLayout
        [ paragraph [ center ] [ text "Products page" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress=Just <| addProductMsgDef, label=text "Add Product"}
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

addProduct dataFieldInput send fail = case (dataFieldInput, send, fail) of
    (productInput, False, Nothing) -> centerXLayout
        [ paragraph [ center ] [ text "Add Products" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Add (PrdInp productInput) True Nothing), label=text "Send"}
        --, paragraph [centerX] [ text "wait" ]
        , inputProducts productInput
        ]
    (_, _, Just msg) -> centerXLayout
        [ paragraph [ center ] [ text "Add Products" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , paragraph [centerX] [ text msg ]
        --, inputProducts (PrdInp productInputDef)
        ]

