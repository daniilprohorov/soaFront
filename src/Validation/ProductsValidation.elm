module Validation.ProductsValidation exposing (..)

import DataField exposing (DataFieldInput(..))
import Debug exposing (toString)
import Dict exposing (Dict)
import List exposing (any)
import Maybe exposing (andThen, withDefault)
import Utils exposing (all, isJust)
import Validation.Fields exposing (validateFloat, validateInt, validateName, validateUnitOfMeasure)

checkDataFieldInputP : DataFieldInput -> Result String (Dict String String)
checkDataFieldInputP dataFieldInput = case dataFieldInput of
   PrdInp products ->
        let
            name = andThen validateName products.name
            x = andThen validateInt products.x
            y = andThen validateFloat products.y
            price = andThen validateInt products.price
            unitofmeasure = andThen validateUnitOfMeasure products.unitofmeasure
            manufacturer = andThen validateInt products.manufacturer

            dataDict = Dict.fromList
                [ ("name", withDefault "" name)
                , ("x", withDefault "" x)
                , ("y", withDefault "" y)
                , ("price", withDefault "" price)
                , ("unitofmeasure", withDefault "" unitofmeasure)
                , ("manufacturer", withDefault "" manufacturer)
                ]
            inpList =
                [ name
                , x
                , y
                , price
                , unitofmeasure
                , manufacturer
                ]
        in
            if all isJust inpList then
                Ok dataDict
            else
                Err <| "input data is not valid"

   _ -> Err "Data field input is not contain product type"

checkDataFieldEditP dataFieldInput = case dataFieldInput of
   PrdInp products ->
        let
            name = andThen validateName products.name
            x = andThen validateInt products.x
            y = andThen validateFloat products.y
            price = andThen validateInt products.price
            unitofmeasure = andThen validateUnitOfMeasure products.unitofmeasure
            manufacturer = andThen validateInt products.manufacturer

            dataDict = Dict.fromList
                [ ("name", withDefault "" name)
                , ("x", withDefault "" x)
                , ("y", withDefault "" y)
                , ("price", withDefault "" price)
                , ("unitofmeasure", withDefault "" unitofmeasure)
                , ("manufacturer", withDefault "" manufacturer)
                ]
            inpList =
                [ name
                , x
                , y
                , price
                , unitofmeasure
                , manufacturer
                ]
        in
            if any isJust inpList then
                Ok dataDict
            else
                Err <| "input data is not valid"

   _ -> Err "Data field input is not contain product type"
