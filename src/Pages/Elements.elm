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
--    Just name -> PageAction <| Add (VehInp {product | name = Just name}) False Nothing
--    Nothing   -> PageAction <| Add (VehInp product) False (Just "Name is not valid")

updateVNameI vehicle str = PageAction <| Add (VehInp {vehicle | name = Just str}) False Nothing
updateVCoordinateXI vehicle str = PageAction <| Add (VehInp {vehicle | coordinate_x = Just str}) False Nothing
updateVCoordinateYI vehicle str = PageAction <| Add (VehInp {vehicle | coordinate_y = Just str}) False Nothing
updateVEnginePowerI vehicle str = PageAction <| Add (VehInp {vehicle | engine_power = Just str}) False Nothing
updateVDistanceTravelledI vehicle str = PageAction <| Add (VehInp {vehicle | distance_travelled = Just str}) False Nothing
updateVVehicleTypeI vehicle str = PageAction <| Add (VehInp {vehicle | vehicle_type = Just str}) False Nothing
updateVFuelTypeI vehicle str = PageAction <| Add (VehInp {vehicle | fuel_type = Just str}) False Nothing




inputVehicleEl vehicleInput =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=updateVNameI vehicleInput
            , text = withDefault "" vehicleInput.name
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "name")
            }
        , Input.text [width <| px 50]
            { onChange=updateVCoordinateXI vehicleInput
            , text = withDefault "" vehicleInput.coordinate_x
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "coordinate_x")
            }
        , Input.text [width <| px 50]
            { onChange=updateVCoordinateYI vehicleInput
            , text = withDefault "" vehicleInput.coordinate_y
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "coordinate_y")
            }
        , Input.text [width <| px 100]
            { onChange=updateVEnginePowerI vehicleInput
            , text = withDefault "" vehicleInput.engine_power
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "engine_power")
            }
        , Input.text [width <| px 150]
            { onChange=updateVDistanceTravelledI vehicleInput
            , text = withDefault "" vehicleInput.distance_travelled
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "distance_travelled")
            }
        , Input.text [width <| px 150]
            { onChange=updateVVehicleTypeI vehicleInput
            , text = withDefault "" vehicleInput.vehicle_type
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "vehicle_type")
            }
        , Input.text [width <| px 150]
            { onChange=updateVFuelTypeI vehicleInput
            , text = withDefault "" vehicleInput.fuel_type
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "fuel_type")
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

updateVIdE str vehicle = case toInt str of
    Just i -> PageAction <| Edit i (VehInp vehicle) False Nothing
    Nothing -> PageAction <| Edit 0 (VehInp vehicle) False (Just "input is not valid")


updateVNameE id vehicle str = PageAction <| Edit id (VehInp {vehicle | name = Just str}) False Nothing
updateVCoordinateXE id vehicle str = PageAction <| Edit id (VehInp {vehicle | coordinate_x = Just str}) False Nothing
updateVCoordinateYE id vehicle str = PageAction <| Edit id (VehInp {vehicle | coordinate_y = Just str}) False Nothing
updateVEnginePowerE id vehicle str = PageAction <| Edit id (VehInp {vehicle | engine_power = Just str}) False Nothing
updateVDistanceTravelledE id vehicle str = PageAction <| Edit id (VehInp {vehicle | distance_travelled = Just str}) False Nothing
updateVVehicleTypeE id vehicle str = PageAction <| Edit id (VehInp {vehicle | vehicle_type = Just str}) False Nothing
updateVFuelTypeE id vehicle str = PageAction <| Edit id (VehInp {vehicle | fuel_type = Just str}) False Nothing

editVehicleEl id vehicleInput =
    row [ centerX, spacing 16 ]
        [ Input.text [width <| px 100]
            { onChange=(\i -> updateVIdE i vehicleInput)
            , text = String.fromInt id
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "id")
            }
        , Input.text [width <| px 100]
            { onChange=(\s -> updateVNameE id vehicleInput s)
            , text = withDefault "" vehicleInput.name
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "name")
            }
        , Input.text [width <| px 50]
            { onChange=updateVCoordinateXE id vehicleInput
            , text = withDefault "" vehicleInput.coordinate_x
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "coordinate_x")
            }
        , Input.text [width <| px 50]
            { onChange=updateVCoordinateYE id vehicleInput
            , text = withDefault "" vehicleInput.coordinate_y
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "coordinate_y")
            }
        , Input.text [width <| px 100]
            { onChange=updateVEnginePowerE id vehicleInput
            , text = withDefault "" vehicleInput.engine_power
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "engine_power")
            }
        , Input.text [width <| px 150]
            { onChange=updateVDistanceTravelledE id vehicleInput
            , text = withDefault "" vehicleInput.distance_travelled
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "distance_travelled")
            }
        , Input.text [width <| px 150]
            { onChange=updateVVehicleTypeE id vehicleInput
            , text = withDefault "" vehicleInput.vehicle_type
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "vehicle_type")
            }
        , Input.text [width <| px 150]
            { onChange=updateVFuelTypeE id vehicleInput
            , text = withDefault "" vehicleInput.fuel_type
            , placeholder = Nothing
            , label = Input.labelAbove [center] (text "fuel_type")
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
