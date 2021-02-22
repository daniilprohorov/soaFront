module Update.OrganizationPageListener exposing (..)

import Dict exposing (get)
import Http as H
import HttpActions exposing (httpAddOrganization, httpDeleteOrganization, httpEditOrganization, httpOrganizations, httpProducts, httpShowOrganization)
import List exposing (filter, map)
import Maybe exposing (withDefault)
import Types exposing (Model(..), Operation(..), defMain)
import Utils exposing (formUrlencoded, isJust)
import Validation.OrganizationsValidation exposing (checkDataFieldEditO, checkDataFieldInputO)

organizationPageUpdate operation = case operation of
    UpdateFilter s f fa sw i p data -> if fa
        then (OrganizationsPage (Main s f False i p) Nothing, httpOrganizations s f fa i p)
        else (OrganizationsPage (Main s f False i p) data, Cmd.none)
    Main s f fa i p-> (OrganizationsPage (Main s f fa i p) Nothing, httpOrganizations s f fa i p)

    Add dataFieldInput False Nothing -> (OrganizationsPage (Add dataFieldInput False Nothing) Nothing , Cmd.none)
    Add dataFieldInput True Nothing -> case checkDataFieldInputO dataFieldInput of
        Ok dataDict ->
            let
                fieldFromDict key = (key, withDefault "" <| get key dataDict)
                data = formUrlencoded
                    [ fieldFromDict "name"
                    , fieldFromDict "fullname"
                    , fieldFromDict "employeescount"
                    ]
                    |> H.stringBody "application/x-www-form-urlencoded"
            in
                (OrganizationsPage defMain Nothing, httpAddOrganization data)

        Err error -> (OrganizationsPage (Add dataFieldInput False (Just error)) Nothing, Cmd.none)

    DeleteById id False fail-> (OrganizationsPage (DeleteById id False fail) Nothing , Cmd.none)
    DeleteById id True Nothing-> (OrganizationsPage defMain Nothing , httpDeleteOrganization id)

    ShowById id False fail data -> (OrganizationsPage (ShowById id False fail data) Nothing , Cmd.none)
    ShowById id True Nothing data -> (OrganizationsPage (ShowById id True Nothing data) Nothing , httpShowOrganization id)


    Edit id dataFieldInput False Nothing -> (OrganizationsPage (Edit id dataFieldInput False Nothing) Nothing , Cmd.none)
    Edit id dataFieldInput True Nothing ->
        case checkDataFieldEditO dataFieldInput of
            Ok dataDict ->
                let
                    fieldFromDict key = (key, get key dataDict)
                    data = formUrlencoded
                        ([ fieldFromDict "name"
                        , fieldFromDict "fullname"
                        , fieldFromDict "employeescount"
                        ] |> filter (\(f, s) -> isJust s) |> map (\(f, s) -> (f, withDefault "" s)))
                        |> H.stringBody "application/x-www-form-urlencoded"
                in
                    (OrganizationsPage defMain Nothing , httpEditOrganization id data)
            Err error -> (OrganizationsPage (Edit id dataFieldInput False (Just error)) Nothing, Cmd.none)

    Edit id dataFieldInput False _ -> (OrganizationsPage (Edit id dataFieldInput False Nothing) Nothing , Cmd.none)


    _ -> Debug.todo "productPageListener error(("
