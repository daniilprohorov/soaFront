module Update.HttpListener exposing (..)

import DataField exposing (DataField(..), DataFieldInput(..))
import Debug exposing (toString)
import HttpActions exposing (httpPriceAvg, httpPriceSum, httpVehicles)
import Organizations exposing (getOrganization, getOrganizations, organizationInputDef)
import Vehicles exposing (getVehicle, getVehicles, vehicleInputDef)
import Types exposing (HttpMsg(..), Model(..), Operation(..), defMain)
import Utils exposing (errorToString)
import Json.Decode exposing (Error(..))

httpUpdate httpMsg model = case httpMsg of
    HttpGetVehicles result filters filterApply elemsperpage page ->
          let
              vehiclesList = case result of
                  Ok fullText -> getVehicles fullText
                  Err e -> Err <| toString e
              vehicles = case vehiclesList of
                  Ok prds -> Just <| Prds prds
                  Err e -> Nothing
          in
              (VehiclesPage (Main Nothing filters filterApply elemsperpage page) vehicles, Cmd.none)

    HttpAddVehicle result -> case result of
        Ok fullText -> ( VehiclesPage defMain Nothing, httpVehicles Nothing Nothing False 20 1 )
        Err e -> (VehiclesPage (Add (VehInp vehicleInputDef) False (Just "SERVER ERROR. Check manufacturer")) Nothing, Cmd.none )

    HttpDeleteVehicle result -> (VehiclesPage defMain Nothing, httpVehicles Nothing Nothing False 20 1 )

    HttpShowVehicle result -> case result of
        Ok res ->
          let
              vehiclesList = case result of
                  Ok fullText -> getVehicle fullText
                  Err e -> Err <| toString e
              product = case vehiclesList of
                  Ok prd -> Just <| Prd prd
                  Err e -> Nothing
          in
             (VehiclesPage (ShowById 0 True Nothing Nothing) product, Cmd.none )
        Err e -> (VehiclesPage (ShowById 0 True (Just (e |> errorToString)) Nothing) Nothing, Cmd.none )

    HttpEditVehicle result -> (VehiclesPage defMain Nothing, httpVehicles Nothing Nothing False 20 1 )

    HttpPriceSum result -> case result of
        Ok res -> (MainPage (Just res) Nothing, httpPriceAvg)
        Err e -> (MainPage (Just "error") Nothing, httpPriceAvg)

    HttpPriceAvg result -> case model of
        MainPage s a -> case result of
            Ok res -> (MainPage s (Just res), Cmd.none)
            Err e -> (MainPage s (Just "error") , Cmd.none)

        VehiclesPage operation maybeDataField -> (MainPage Nothing Nothing, httpPriceSum)



