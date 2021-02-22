module Validation.OrganizationsValidation exposing (..)

import DataField exposing (DataFieldInput(..))
import Dict exposing (Dict)
import List exposing (any)
import Maybe exposing (andThen, withDefault)
import Utils exposing (all, isJust)
import Validation.Fields exposing (validateInt, validateName)

checkDataFieldInputO : DataFieldInput -> Result String (Dict String String)
checkDataFieldInputO dataFieldInput = case dataFieldInput of
   OrgInp organization ->
        let
            name = andThen validateName organization.name
            fullname = andThen validateName organization.fullname
            employeescount = andThen validateInt organization.employeescount

            dataDict = Dict.fromList
                [ ("name", withDefault "" name)
                , ("fullname", withDefault "" fullname)
                , ("employeescount", withDefault "" employeescount)
                ]
            inpList =
                [ name
                , fullname
                , employeescount
                ]
        in
            if all isJust inpList then
                Ok dataDict
            else
                Err <| "input data is not valid"

   _ -> Err "Data field input is not contain organization type"

checkDataFieldEditO dataFieldInput = case dataFieldInput of
   OrgInp organization ->
        let
            name = andThen validateName organization.name
            fullname = andThen validateName organization.fullname
            employeescount = andThen validateInt organization.employeescount

            dataDict = Dict.fromList
                [ ("name", withDefault "" name)
                , ("fullname", withDefault "" fullname)
                , ("employeescount", withDefault "" employeescount)
                ]
            inpList =
                [ name
                , fullname
                , employeescount
                ]

        in
            if any isJust inpList then
                Ok dataDict
            else
                Err <| "input data is not valid"

   _ -> Err "Data field input is not contain organization type"
