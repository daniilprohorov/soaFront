module Pages.ProductsPage exposing (..)

import DataField exposing (DataField(..), DataFieldInput(..))
import Element exposing (centerX, paragraph, text)
import Element.Font exposing (center)
import Element.Input as Input exposing (button)
import Pages.Elements exposing (deleteProductEl, editProductEl, inputProductEl)
import Pages.Layouts exposing (centerLayout, centerXLayout, twoColumnsLayout)
import Pages.Styles exposing (buttonStyle)
import Products exposing (printProducts, productInputDef, Product)
import Types exposing (Msg(..), Operation(..), ToPage(..), addProductMsgDef, defMain)

showProductsPage operation data = case operation of
    Main _ _ _ _-> mainProducts data
    Add (PrdInp dataFieldInput) send fail -> addProduct dataFieldInput send fail
    Edit id (PrdInp dataFieldInput) send fail -> editProduct id dataFieldInput send fail
    DeleteById id _ fail -> deleteProduct id fail
    _ -> Debug.todo "lul"

mainProducts data = case data of
    Just (Prds products) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Products" ]
        , button buttonStyle {onPress=Just <| addProductMsgDef, label=text "Add Product"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress=Just <| PageAction (DeleteById 0 False Nothing), label=text "Delete by Id"}
        , button buttonStyle {onPress=Just <| PageAction (Edit 0 (PrdInp productInputDef) False Nothing), label=text "Edit"}
        ]
        [ paragraph [centerX] [ printProducts products ]
        ]

    Just _ -> Debug.todo "show products error"

    Nothing -> twoColumnsLayout
        [ paragraph [ center ] [ text "Products" ]
        , button buttonStyle {onPress=Just <| addProductMsgDef, label=text "Add Product"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        ]
        [ paragraph [centerX] [ text "wait" ]
        ]

addProduct dataFieldInput send fail = case (dataFieldInput, send, fail) of
    (productInput, False, Nothing) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Products" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Products main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Add (PrdInp productInput) True Nothing), label=text "Send"}
        ]
        [ inputProductEl productInput
        ]
    (productInput, _, Just msg) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Add Products" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Products main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Add (PrdInp productInput) True Nothing), label=text "Send"}
        ]
        [ inputProductEl productInput
        , paragraph [centerX] [ text msg ]
        ]

    (_, True, Nothing) -> Debug.todo "Error in add product"

deleteProduct id fail = case fail of
    Nothing -> twoColumnsLayout
        [ paragraph [ center ] [ text "Delete Product" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Products main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (DeleteById id True Nothing), label=text "Delete"}
        ]
        [ deleteProductEl id
        ]

    Just e -> twoColumnsLayout
        [ paragraph [ center ] [ text "Delete Product" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Products main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (DeleteById id True Nothing), label=text "Delete"}
        ]
        [ deleteProductEl id
        , paragraph [centerX] [ text e ]
        ]

editProduct id dataFieldInput send fail = case (dataFieldInput, send, fail) of
    (productInput, False, Nothing) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Edit product" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Products main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Edit id (PrdInp productInput) True Nothing), label=text "Send"}
        ]
        [ editProductEl id productInput
        ]
    (productInput, _, Just msg) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Edit products" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Products main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Edit id (PrdInp productInput) True Nothing), label=text "Send"}
        ]
        [ editProductEl id productInput
        , paragraph [centerX] [ text msg ]
        ]

    (_, True, Nothing) -> Debug.todo "Error in add product"
