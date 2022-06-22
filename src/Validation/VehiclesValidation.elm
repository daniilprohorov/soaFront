module Validation.VehiclesValidation exposing (..)

import DataField exposing (DataFieldInput(..))
import Debug exposing (toString)
import Dict exposing (Dict)
import List exposing (any)
import Maybe exposing (andThen, withDefault)
import Utils exposing (all, isJust)
import Validation.Fields exposing (validateFloat, validateInt, validateName, validateVehicleType, validateFuelType)

checkDataFieldInputP : DataFieldInput -> Result String (Dict String String)
checkDataFieldInputP dataFieldInput = case dataFieldInput of
   VehInp vehicles ->
        let
            name = andThen validateName vehicles.name
            coordinate_x = andThen validateFloat vehicles.coordinate_x
            coordinate_y = andThen validateInt vehicles.coordinate_y
            engine_power = andThen validateInt vehicles.engine_power
            distance_travelled = andThen validateFloat vehicles.distance_travelled
            vehicle_type = andThen validateVehicleType vehicles.vehicle_type
            fuel_type = andThen validateFuelType vehicles.fuel_type

            dataDict = Dict.fromList
                [ ("name", withDefault "" name)
                , ("coordinate_x", withDefault "" coordinate_x)
                , ("coordinate_y", withDefault "" coordinate_y)
                , ("engine_power", withDefault "" engine_power)
                , ("distance_travelled", withDefault "" distance_travelled)
                , ("vehicle_type", withDefault "" vehicle_type)
                , ("fuel_type", withDefault "" fuel_type)
                ]
            inpList =
                [ name
                , coordinate_x
                , coordinate_y
                , engine_power
                , distance_travelled
                , vehicle_type
                , fuel_type
                ]
        in
            if all isJust inpList then
                Ok dataDict
            else
                Err <| "input data is not valid"

checkDataFieldEditP dataFieldInput = case dataFieldInput of
   VehInp vehicles ->
        let
            name = andThen validateName vehicles.name
            coordinate_x = andThen validateFloat vehicles.coordinate_x
            coordinate_y = andThen validateInt vehicles.coordinate_y
            engine_power = andThen validateInt vehicles.engine_power
            distance_travelled = andThen validateFloat vehicles.distance_travelled
            vehicle_type = andThen validateVehicleType vehicles.vehicle_type
            fuel_type = andThen validateFuelType vehicles.fuel_type

            dataDict = Dict.fromList
                [ ("name", withDefault "" name)
                , ("coordinate_x", withDefault "" coordinate_x)
                , ("coordinate_y", withDefault "" coordinate_y)
                , ("engine_power", withDefault "" engine_power)
                , ("distance_travelled", withDefault "" distance_travelled)
                , ("vehicle_type", withDefault "" vehicle_type)
                , ("fuel_type", withDefault "" fuel_type)
                ]
            inpList =
                [ name
                , coordinate_x
                , coordinate_y
                , engine_power
                , distance_travelled
                , vehicle_type
                , fuel_type
                ]
        in
            if any isJust inpList then
                Ok dataDict
            else
                Err <| "input data is not valid"
