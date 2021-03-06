module Update.ProductPageListener exposing (..)

import Dict exposing (get)
import Http as H
import HttpActions exposing (httpAddProduct, httpDeleteProduct, httpEditProduct, httpProducts, httpProductsP, httpShowProduct)
import List exposing (filter, map)
import Maybe exposing (andThen, withDefault)
import Types exposing (Model(..), Msg(..), Operation(..), defMain)
import Utils exposing (isJust, xmlEncoder)
import Validation.Fields exposing (validateFilter, validateName)
import Validation.ProductsValidation exposing (checkDataFieldEditP, checkDataFieldInputP)

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
                            (ProductsPage (Main s filterV False i p) Nothing, httpProducts s filterV False i p)
                    just -> (ProductsPage (Main s just False i p) Nothing, httpProducts s just fa i p)
        (_, True) ->
            let
                maybeFilter = andThen validateName f

            in
                case maybeFilter of
                    Nothing ->
                        let
                            filterV = Just "INPUT: Letters"
                        in
                            (ProductsPage (Main s filterV False i p) Nothing, httpProducts s filterV False i p)
                    just -> (ProductsPage (Main s just False i p) Nothing, httpProductsP s just fa i p)

        (_, _) ->(ProductsPage (Main s f False i p) data, Cmd.none)
    Main s f fa i p-> (ProductsPage (Main s f fa i p) Nothing, httpProducts s f fa i p)

    Add dataFieldInput False Nothing -> (ProductsPage (Add dataFieldInput False Nothing) Nothing , Cmd.none)
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
                (ProductsPage defMain Nothing, httpAddProduct data)

        Err error -> (ProductsPage (Add dataFieldInput False (Just error)) Nothing, Cmd.none)

    DeleteById id False fail-> (ProductsPage (DeleteById id False fail) Nothing , Cmd.none)
    DeleteById id True Nothing-> (ProductsPage defMain Nothing , httpDeleteProduct id)

    ShowById id False fail data -> (ProductsPage (ShowById id False fail data) Nothing , Cmd.none)
    ShowById id True Nothing data -> (ProductsPage (ShowById id True Nothing data) Nothing , httpShowProduct id)


    Edit id dataFieldInput False Nothing -> (ProductsPage (Edit id dataFieldInput False Nothing) Nothing , Cmd.none)
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
                    (ProductsPage defMain Nothing , httpEditProduct id data)
            Err error -> (ProductsPage (Edit id dataFieldInput False (Just error)) Nothing, Cmd.none)

    Edit id dataFieldInput False _ -> (ProductsPage (Edit id dataFieldInput False Nothing) Nothing , Cmd.none)



    _ -> Debug.todo "productPageListener lul"
