module Update.ProductPageListener exposing (..)

import Dict exposing (get)
import Http as H
import HttpActions exposing (httpAddProduct, httpDeleteProduct, httpEditProduct, httpProducts)
import List exposing (filter, map)
import Maybe exposing (withDefault)
import Types exposing (Model(..), Msg(..), Operation(..), defMain)
import Utils exposing (formUrlencoded, isJust)
import Validation.ProductsValidation exposing (checkDataFieldEditP, checkDataFieldInputP)

productPageUpdate operation = case operation of

    Main s f i p-> (ProductsPage (Main s f i p) Nothing, httpProducts s f i p)
    Add dataFieldInput False Nothing -> (ProductsPage (Add dataFieldInput False Nothing) Nothing , Cmd.none)
    Add dataFieldInput True Nothing -> case checkDataFieldInputP dataFieldInput of
        Ok dataDict ->
            let
                fieldFromDict key = (key, withDefault "" <| get key dataDict)
                data = formUrlencoded
                    [ fieldFromDict "name"
                    , fieldFromDict "x"
                    , fieldFromDict "y"
                    , fieldFromDict "price"
                    , fieldFromDict "unitofmeasure"
                    , fieldFromDict "manufacturer"
                    ]
                    |> H.stringBody "application/x-www-form-urlencoded"
            in
                (ProductsPage defMain Nothing, httpAddProduct data)

        Err error -> (ProductsPage (Add dataFieldInput False (Just error)) Nothing, Cmd.none)

    DeleteById id False fail-> (ProductsPage (DeleteById id False fail) Nothing , Cmd.none)
    DeleteById id True Nothing-> (ProductsPage defMain Nothing , httpDeleteProduct id)


    Edit id dataFieldInput False Nothing -> (ProductsPage (Edit id dataFieldInput False Nothing) Nothing , Cmd.none)
    Edit id dataFieldInput True Nothing ->
        case checkDataFieldEditP dataFieldInput of
            Ok dataDict ->
                let
                    fieldFromDict key = (key, get key dataDict)
                    data = formUrlencoded
                        ([ fieldFromDict "name"
                        , fieldFromDict "x"
                        , fieldFromDict "y"
                        , fieldFromDict "price"
                        , fieldFromDict "unitofmeasure"
                        , fieldFromDict "manufacturer"
                        ] |> filter (\(f, s) -> isJust s) |> map (\(f, s) -> (f, withDefault "" s)))
                        |> H.stringBody "application/x-www-form-urlencoded"
                in
                    (ProductsPage defMain Nothing , httpEditProduct id data)
            Err error -> (ProductsPage (Edit id dataFieldInput False (Just error)) Nothing, Cmd.none)



    _ -> Debug.todo "productPageListener lul"
