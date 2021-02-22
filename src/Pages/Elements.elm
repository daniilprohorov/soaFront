module Pages.Elements exposing (..)

import DataField exposing (DataField(..), DataFieldInput(..))
import Element exposing (centerX, padding, px, row, spacing, text, width)
import Element.Font exposing (center)
import Element.Input as Input
import Maybe exposing (withDefault)
import String exposing (toInt)
import Types exposing (Msg(..), Operation(..))
import Validation.Fields exposing (validateName)

--updateName product str = case validateName str of
--    Just name -> PageAction <| Add (PrdInp {product | name = Just name}) False Nothing
--    Nothing   -> PageAction <| Add (PrdInp product) False (Just "Name is not valid")

updatePNameI product str = PageAction <| Add (PrdInp {product | name = Just str}) False Nothing
updatePXI product str = PageAction <| Add (PrdInp {product | x = Just str}) False Nothing
updatePYI product str = PageAction <| Add (PrdInp {product | y = Just str}) False Nothing
updatePPriceI product str = PageAction <| Add (PrdInp {product | price = Just str}) False Nothing
updatePUnitOfMeasureI product str = PageAction <| Add (PrdInp {product | unitofmeasure = Just str}) False Nothing
updatePManufacturerI product str = PageAction <| Add (PrdInp {product | manufacturer = Just str}) False Nothing

updateONameI organization str = PageAction <| Add (OrgInp {organization | name = Just str}) False Nothing
updateOFullNameI organization str = PageAction <| Add (OrgInp {organization | fullname = Just str}) False Nothing
updateOEmployeesCountI organizatoin str = PageAction <| Add (OrgInp {organizatoin | employeescount = Just str}) False Nothing




inputProductEl productInput =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=updatePNameI productInput
            , text = withDefault "" productInput.name
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Name")
            }
        , Input.text [width <| px 50]
            { onChange=updatePXI productInput
            , text = withDefault "" productInput.x
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "X")
            }
        , Input.text [width <| px 50]
            { onChange=updatePYI productInput
            , text = withDefault "" productInput.y
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Y")
            }
        , Input.text [width <| px 100]
            { onChange=updatePPriceI productInput
            , text = withDefault "" productInput.price
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Price")
            }
        , Input.text [width <| px 150]
            { onChange=updatePUnitOfMeasureI productInput
            , text = withDefault "" productInput.unitofmeasure
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Unit of measure")
            }
        , Input.text [width <| px 150]
            { onChange=updatePManufacturerI productInput
            , text = withDefault "" productInput.manufacturer
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Manufacturer")
            }
        ]

inputOrganizationEl organizationInput =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=updateONameI organizationInput
            , text = withDefault "" organizationInput.name
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Name")
            }
        , Input.text [width <| px 200]
            { onChange=updateOFullNameI organizationInput
            , text = withDefault "" organizationInput.fullname
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Fullname")
            }
        , Input.text [width <| px 50]
            { onChange=updateOEmployeesCountI organizationInput
            , text = withDefault "" organizationInput.employeescount
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Employeescount")
            }
        ]


updateIdD str = case toInt str of
    Just i -> PageAction <| (DeleteById i ) False Nothing
    Nothing -> PageAction <| (DeleteById 0 ) False (Just "input is not valid")

updateIdS str = case toInt str of
    Just i -> PageAction <| (ShowById i ) False Nothing Nothing
    Nothing -> PageAction <| (ShowById 0 ) False (Just "input is not valid") Nothing

deleteEl id =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=updateIdD
            , text = String.fromInt id
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Id")
            }
        ]

showByIdEl id =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=updateIdS
            , text = String.fromInt id
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Id")
            }
        ]

updatePIdE str product = case toInt str of
    Just i -> PageAction <| Edit i (PrdInp product) False Nothing
    Nothing -> PageAction <| Edit 0 (PrdInp product) False (Just "input is not valid")

updateOIdE str organization = case toInt str of
    Just i -> PageAction <| Edit i (OrgInp organization) False Nothing
    Nothing -> PageAction <| Edit 0 (OrgInp organization) False (Just "input is not valid")

updatePNameE id product str = PageAction <| Edit id (PrdInp {product | name = Just str}) False Nothing
updatePXE id product str = PageAction <| Edit id (PrdInp {product | x = Just str}) False Nothing
updatePYE id product str = PageAction <| Edit id (PrdInp {product | y = Just str}) False Nothing
updatePPriceE id product str = PageAction <| Edit id (PrdInp {product | price = Just str}) False Nothing
updatePUnitOfMeasureE id product str = PageAction <| Edit id (PrdInp {product | unitofmeasure = Just str}) False Nothing
updatePManufacturerE id product str = PageAction <| Edit id (PrdInp {product | manufacturer = Just str}) False Nothing

updateONameE id organization str = PageAction <| Edit id (OrgInp {organization | name = Just str}) False Nothing
updateOFullNameE id organization str = PageAction <| Edit id (OrgInp {organization | fullname = Just str}) False Nothing
updateOEmployeesCountNameE id organization str = PageAction <| Edit id (OrgInp {organization | employeescount = Just str}) False Nothing

editProductEl id productInput =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=(\i -> updatePIdE i productInput)
            , text = String.fromInt id
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Id")
            }
        , Input.text [width <| px 100]
            { onChange=(\s -> updatePNameE id productInput s)
            , text = withDefault "" productInput.name
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Name")
            }
        , Input.text [width <| px 50]
            { onChange=updatePXE id productInput
            , text = withDefault "" productInput.x
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "X")
            }
        , Input.text [width <| px 50]
            { onChange=updatePYE id productInput
            , text = withDefault "" productInput.y
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Y")
            }
        , Input.text [width <| px 100]
            { onChange=updatePPriceE id productInput
            , text = withDefault "" productInput.price
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Price")
            }
        , Input.text [width <| px 150]
            { onChange=updatePUnitOfMeasureE id productInput
            , text = withDefault "" productInput.unitofmeasure
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Unit of measure")
            }
        , Input.text [width <| px 150]
            { onChange=updatePManufacturerE id productInput
            , text = withDefault "" productInput.manufacturer
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Manufacturer")
            }
        ]

editOrganizationEl id organizationInput =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=(\i -> updateOIdE i organizationInput)
            , text = String.fromInt id
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Id")
            }
        , Input.text [width <| px 100]
            { onChange=(\s -> updateONameE id organizationInput s)
            , text = withDefault "" organizationInput.name
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Name")
            }
        , Input.text [width <| px 100]
            { onChange=(\s -> updateOFullNameE id organizationInput s)
            , text = withDefault "" organizationInput.fullname
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "FullName")
            }
        , Input.text [width <| px 100]
            { onChange=(\s -> updateOEmployeesCountNameE id organizationInput s)
            , text = withDefault "" organizationInput.employeescount
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Employees count")
            }
        ]

--radioButtons = Input.radio
--    [ padding 10
--    , spacing 20
--    ]
--    { onChange =
--    , selected = Just model.lunch
--    , label = Input.labelAbove [] (text "Lunch")
--    , options =
--        [ Input.option Burrito (text "Burrito")
--        , Input.option Taco (text "Taco!")
--        , Input.option Gyro (text "Gyro")
--        ]
--    }
