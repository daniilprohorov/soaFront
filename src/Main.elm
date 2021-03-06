module Main exposing (main)

import DataField exposing (DataField(..), DataFieldInput(..))
import Debug exposing (toString)
import Dict exposing (get)
import Http exposing (header)
import Browser
import Html exposing (Html, div, tbody, td, th, thead, tr)
import HttpActions exposing (httpAddProduct, httpOrganizations, httpPriceSum, httpProducts)
import Maybe exposing (withDefault)
import Organizations exposing (getOrganizations, printOrganizations)
import Pages.MainPage exposing (showMainPage)
import Pages.OrganizationsPage exposing (showOrganizationsPage)
import Pages.ProductsPage exposing (showProductsPage)
import Products exposing (getProducts, printProducts)
import Result exposing (andThen)

import Update.Update exposing (update)
import Utils exposing (formUrlencoded)
import Validation.ProductsValidation exposing (checkDataFieldInputP)


import Types exposing (..)
import Xml.Decode as Encode
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> (Model, Cmd Msg)
init _ =
  ( MainPage Nothing Nothing
  , httpPriceSum
  )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  case model of
      MainPage s a-> showMainPage s a
      ProductsPage operation data -> showProductsPage operation data
      OrganizationsPage operation data -> showOrganizationsPage operation data
