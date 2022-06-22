module Update.VehiclePageListener exposing (..)

import Dict exposing (get)
import Http as H
import HttpActions exposing (httpAddVehicle, httpDeleteVehicle, httpEditVehicle, httpVehicles, httpVehiclesP, httpShowVehicle)
import List exposing (filter, map)
import Maybe exposing (andThen, withDefault)
import Types exposing (Model(..), Msg(..), Operation(..), defMain)
import Utils exposing (isJust, xmlEncoder)
import Validation.Fields exposing (validateFilter, validateName)
import Validation.VehiclesValidation exposing (checkDataFieldEditP, checkDataFieldInputP)

productPageUpdate operation = case operation of
    UpdateFilter s f fa sw i p data -> case (fa, sw) of
        (True, _) ->
            let
                maybeFilter = andThen validateFilter f
            in
                case maybeFilter of
                    Nothing ->
                        let
                            filterV = Just "INPUT: Letters, digits, =, -, &, ."
                        in
                            (VehiclesPage (Main s filterV False i p) Nothing, httpVehicles s filterV False i p)
                    just -> (VehiclesPage (Main s just False i p) Nothing, httpVehicles s just fa i p)
        (_, True) ->
            let
                maybeFilter = andThen validateName f

            in
                case maybeFilter of
                    Nothing ->
                        let
                            filterV = Just "INPUT: Letters"
                        in
                            (VehiclesPage (Main s filterV False i p) Nothing, httpVehicles s filterV False i p)
                    just -> (VehiclesPage (Main s just False i p) Nothing, httpVehiclesP s just fa i p)

        (_, _) ->(VehiclesPage (Main s f False i p) data, Cmd.none)
    Main s f fa i p-> (VehiclesPage (Main s f fa i p) Nothing, httpVehicles s f fa i p)

    Add dataFieldInput False Nothing -> (VehiclesPage (Add dataFieldInput False Nothing) Nothing , Cmd.none)
    Add dataFieldInput True Nothing -> case checkDataFieldInputP dataFieldInput of
        Ok dataDict ->
            let
                fieldFromDict key = (key, withDefault "" <| get key dataDict)
                data = xmlEncoder
                    [ fieldFromDict "name"
                    , fieldFromDict "x"
                    , fieldFromDict "y"
                    , fieldFromDict "price"
                    , fieldFromDict "unitofmeasure"
                    , fieldFromDict "manufacturer"
                    ]
                    |> H.stringBody "text/xml;charset=utf-8"
            in
                (VehiclesPage defMain Nothing, httpAddVehicle data)

        Err error -> (VehiclesPage (Add dataFieldInput False (Just error)) Nothing, Cmd.none)

    DeleteById id False fail-> (VehiclesPage (DeleteById id False fail) Nothing , Cmd.none)
    DeleteById id True Nothing-> (VehiclesPage defMain Nothing , httpDeleteVehicle id)

    ShowById id False fail data -> (VehiclesPage (ShowById id False fail data) Nothing , Cmd.none)
    ShowById id True Nothing data -> (VehiclesPage (ShowById id True Nothing data) Nothing , httpShowVehicle id)


    Edit id dataFieldInput False Nothing -> (VehiclesPage (Edit id dataFieldInput False Nothing) Nothing , Cmd.none)
    Edit id dataFieldInput True Nothing ->
        case checkDataFieldEditP dataFieldInput of
            Ok dataDict ->
                let
                    fieldFromDict key = (key, get key dataDict)
                    data = xmlEncoder
                        ([ fieldFromDict "name"
                        , fieldFromDict "x"
                        , fieldFromDict "y"
                        , fieldFromDict "price"
                        , fieldFromDict "unitofmeasure"
                        , fieldFromDict "manufacturer"
                        ] |> filter (\(f, s) -> isJust s) |> map (\(f, s) -> (f, withDefault "" s)))
                        |> H.stringBody "text/xml;charset=utf-8"
                in
                    (VehiclesPage defMain Nothing , httpEditVehicle id data)
            Err error -> (VehiclesPage (Edit id dataFieldInput False (Just error)) Nothing, Cmd.none)

    Edit id dataFieldInput False _ -> (VehiclesPage (Edit id dataFieldInput False Nothing) Nothing , Cmd.none)



    _ -> Debug.todo "productPageListener lul"
