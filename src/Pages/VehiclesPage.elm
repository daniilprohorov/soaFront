module Pages.VehiclesPage exposing (..)

import DataField exposing (DataField(..), DataFieldInput(..))
import Element exposing (centerX, column, paragraph, px, text, width)
import Element.Font exposing (center)
import Element.Input as Input exposing (button)
import Maybe exposing (withDefault)
import Pages.Elements exposing (deleteEl, editVehicleEl, inputVehicleEl, showByIdEl)
import Pages.Layouts exposing (centerLayout, centerXLayout, twoColumnsLayout)
import Pages.Styles exposing (buttonStyle)
import Vehicles exposing (printVehicles, vehicleInputDef, Vehicle)
import Types exposing (Msg(..), Operation(..), ToPage(..), addProductMsgDef, defMain)

showVehiclesPage operation data = case operation of
    Main sort filter filterApply elemsPerPage page-> mainVehicles data sort filter filterApply elemsPerPage page
    Add (VehInp dataFieldInput) send fail -> addVehicle dataFieldInput send fail
    Edit id (VehInp dataFieldInput) send fail -> editVehicle id dataFieldInput send fail
    DeleteById id _ fail -> deleteVehicle id fail
    ShowById id _ fail res -> showByIdVehicle id fail data
    _ -> Debug.todo "lul"

mainVehicles data sort filter filterApply elemsPerPage page = case data of
    Just (Prds vehicles) -> twoColumnsLayout
        [ paragraph [ center ] [ text "vehicles" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , column buttonStyle
            [ paragraph [center] [text "Sort"]
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "id") filter filterApply elemsPerPage page), label=text"id"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "name") filter filterApply elemsPerPage page), label=text"name"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "x") filter filterApply elemsPerPage page), label=text"x"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "y") filter filterApply elemsPerPage page), label=text"y"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "creationdate") filter filterApply elemsPerPage page), label=text"date"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "price") filter filterApply elemsPerPage page), label=text"price"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "unitofmeasure") filter filterApply elemsPerPage page), label=text"unit of measure"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "manufacturer") filter filterApply elemsPerPage page), label=text"manufacturer"}
            ]
        , column buttonStyle
            [ Input.text [width <| px 400]
                { onChange=(\s -> PageAction (UpdateFilter sort (Just s) False False elemsPerPage page data))
                , text = withDefault "" filter
                , placeholder = Nothing
                , label = Input.labelAbove [center] (text "FILTER VALUE")
                }
            , button buttonStyle {onPress=Just <| PageAction (UpdateFilter sort filter True False elemsPerPage page data), label=text"APPLY"}
            , button buttonStyle {onPress=Just <| PageAction (UpdateFilter sort filter False True elemsPerPage page data), label=text"Start With"}
            ]
        , button buttonStyle {onPress=Just <| addProductMsgDef, label=text "Add Product"}
        , button buttonStyle {onPress=Just <| PageAction (DeleteById 0 False Nothing), label=text "Delete by Id"}
        , button buttonStyle {onPress=Just <| PageAction (Edit 0 (VehInp vehicleInputDef) False Nothing), label=text "Edit"}
        , button buttonStyle {onPress=Just <| PageAction (ShowById 0 False Nothing Nothing), label=text "Show by id"}
        , paragraph [center]
            [ text <| "Elems per page = " ++ (String.fromInt elemsPerPage)
            , button buttonStyle {onPress=Just <| PageAction (Main sort filter filterApply (elemsPerPage + 1) page), label=text"+"}
            , button buttonStyle {onPress=Just <| PageAction
                (if elemsPerPage >= 2
                    then Main sort filter filterApply (elemsPerPage - 1) page
                    else Main sort filter filterApply elemsPerPage page
                ), label=text"-"}
            ]

        , paragraph [center]
            [ text <| "Page = " ++ (String.fromInt page)
            , button buttonStyle {onPress=Just <| PageAction
                (if page >= 2
                    then Main sort filter filterApply elemsPerPage (page - 1)
                    else Main sort filter filterApply elemsPerPage page
                ), label=text"<"}
            , button buttonStyle {onPress=Just <| PageAction (Main sort filter filterApply elemsPerPage (page+1)), label=text">"}
            ]
        ]
        [ paragraph [centerX] [ printVehicles vehicles ]
        ]

    Just _ -> Debug.todo "show vehicles error"

    Nothing -> twoColumnsLayout
        [ paragraph [ center ] [ text "vehicles" ]
        , button buttonStyle {onPress=Just <| addProductMsgDef, label=text "Add Product"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        ]
        [ paragraph [centerX] [ text "ERROR" ]
        ]

addVehicle dataFieldInput send fail = case (dataFieldInput, send, fail) of
    (vehicleInput, False, Nothing) -> twoColumnsLayout
        [ paragraph [ center ] [ text "vehicles" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "vehicles main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Add (VehInp vehicleInput) True Nothing), label=text "Send"}
        ]
        [ inputVehicleEl vehicleInput
        ]
    (vehicleInput, _, Just msg) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Add vehicles" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "vehicles main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Add (VehInp vehicleInput) True Nothing), label=text "Send"}
        ]
        [ inputVehicleEl vehicleInput
        , paragraph [centerX] [ text msg ]
        ]

    (_, True, Nothing) -> Debug.todo "Error in add product"

editVehicle id dataFieldInput send fail = case (dataFieldInput, send, fail) of
    (vehicleInput, False, Nothing) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Edit product" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "vehicles main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Edit id (VehInp vehicleInput) True Nothing), label=text "Send"}
        ]
        [ editVehicleEl id vehicleInput
        ]
    (vehicleInput, _, Just msg) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Edit vehicles" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "vehicles main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Edit id (VehInp vehicleInput) True Nothing), label=text "Send"}
        ]
        [ editVehicleEl id vehicleInput
        , paragraph [centerX] [ text msg ]
        ]

    (_, True, Nothing) -> Debug.todo "Error in add product"

deleteVehicle id fail = case fail of
    Nothing -> twoColumnsLayout
        [ paragraph [ center ] [ text "Delete Product" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "vehicles main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (DeleteById id True Nothing), label=text "Delete"}
        ]
        [ deleteEl id
        ]

    Just e -> twoColumnsLayout
        [ paragraph [ center ] [ text "Delete Product" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "vehicles main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (DeleteById id True Nothing), label=text "Delete"}
        ]
        [ deleteEl id
        , paragraph [centerX] [ text e ]
        ]

showByIdVehicle id fail data = case (fail, data) of
    (Nothing, Nothing) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Show product by id" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "vehicles main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (ShowById id True Nothing Nothing), label=text "Show"}
        ]
        [ showByIdEl id
        ]

    (Nothing, Just (Prd product)) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Show product by id" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "vehicles main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        ]

        [ paragraph [centerX] [ printVehicles [product] ]
        ]
    (Nothing, Just (Org _)) -> Debug.todo "only vehicles"
    (Nothing, Just (Prds _)) -> Debug.todo "only vehicles"
    (Nothing, Just (Orgs _)) -> Debug.todo "only vehicles"

    (Just e, _) -> twoColumnsLayout [ paragraph [ center ] [ text "Show product by id" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "vehicles main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (ShowById id True Nothing Nothing), label=text "Show"}
        ]
        [ showByIdEl id
        , paragraph [centerX] [ text e ]
        ]

