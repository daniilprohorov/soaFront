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

updateNameI product str = PageAction <| Add (PrdInp {product | name = Just str}) False Nothing
updateXI product str = PageAction <| Add (PrdInp {product | x = Just str}) False Nothing
updateYI product str = PageAction <| Add (PrdInp {product | y = Just str}) False Nothing
updatePriceI product str = PageAction <| Add (PrdInp {product | price = Just str}) False Nothing
updateUnitOfMeasureI product str = PageAction <| Add (PrdInp {product | unitofmeasure = Just str}) False Nothing
updateManufacturerI product str = PageAction <| Add (PrdInp {product | manufacturer = Just str}) False Nothing



inputProductEl productInput =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=updateNameI productInput
            , text = withDefault "" productInput.name
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Name")
            }
        , Input.text [width <| px 50]
            { onChange=updateXI productInput
            , text = withDefault "" productInput.x
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "X")
            }
        , Input.text [width <| px 50]
            { onChange=updateYI productInput
            , text = withDefault "" productInput.y
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Y")
            }
        , Input.text [width <| px 100]
            { onChange=updatePriceI productInput
            , text = withDefault "" productInput.price
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Price")
            }
        , Input.text [width <| px 150]
            { onChange=updateUnitOfMeasureI productInput
            , text = withDefault "" productInput.unitofmeasure
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Unit of measure")
            }
        , Input.text [width <| px 150]
            { onChange=updateManufacturerI productInput
            , text = withDefault "" productInput.manufacturer
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Manufacturer")
            }
        ]


updateIdD str = case toInt str of
    Just i -> PageAction <| (DeleteById i ) False Nothing
    Nothing -> PageAction <| (DeleteById 0 ) False (Just "input is not valid")

deleteProductEl id =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=updateIdD
            , text = String.fromInt id
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Id")
            }
        ]

updateIdE str product = case toInt str of
    Just i -> PageAction <| Edit i (PrdInp product) False Nothing
    Nothing -> PageAction <| Edit 0 (PrdInp product) False (Just "input is not valid")

updateNameE id product str = PageAction <| Edit id (PrdInp {product | name = Just str}) False Nothing
updateXE id product str = PageAction <| Edit id (PrdInp {product | x = Just str}) False Nothing
updateYE id product str = PageAction <| Edit id (PrdInp {product | y = Just str}) False Nothing
updatePriceE id product str = PageAction <| Edit id (PrdInp {product | price = Just str}) False Nothing
updateUnitOfMeasureE id product str = PageAction <| Edit id (PrdInp {product | unitofmeasure = Just str}) False Nothing
updateManufacturerE id product str = PageAction <| Edit id (PrdInp {product | manufacturer = Just str}) False Nothing

editProductEl id productInput =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=(\i -> updateIdE i productInput)
            , text = String.fromInt id
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Id")
            }
        , Input.text [width <| px 100]
            { onChange=(\s -> updateNameE id productInput s)
            , text = withDefault "" productInput.name
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Name")
            }
        , Input.text [width <| px 50]
            { onChange=updateXE id productInput
            , text = withDefault "" productInput.x
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "X")
            }
        , Input.text [width <| px 50]
            { onChange=updateYE id productInput
            , text = withDefault "" productInput.y
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Y")
            }
        , Input.text [width <| px 100]
            { onChange=updatePriceE id productInput
            , text = withDefault "" productInput.price
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Price")
            }
        , Input.text [width <| px 150]
            { onChange=updateUnitOfMeasureE id productInput
            , text = withDefault "" productInput.unitofmeasure
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Unit of measure")
            }
        , Input.text [width <| px 150]
            { onChange=updateManufacturerE id productInput
            , text = withDefault "" productInput.manufacturer
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "Manufacturer")
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
