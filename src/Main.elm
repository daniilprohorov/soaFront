module Main exposing (main)

import DataField exposing (DataField(..), DataFieldInput(..))
import Debug exposing (toString)
import Element exposing (alignRight, centerX, centerY, column, el, fill, layout, padding, paragraph, rgb255, row, spacing, spacingXY, table, text, width)
import Element.Border as Border
import Element.Font exposing (center, size)
import Element.Input as Input exposing (button)
import Html.Attributes exposing (class)
import Http exposing (header)
import Browser
import Html exposing (Html, div, tbody, td, th, thead, tr)
import Html.Events exposing (onClick)
import HttpActions exposing (httpAddProduct, httpOrganizations, httpProducts)
import List exposing (all, map)
import Maybe exposing (withDefault)
import Organizations exposing (getOrganizations, printOrganizations)
import Pages.MainPage exposing (showMainPage)
import Pages.OrganizationsPage exposing (showOrganizationsPage)
import Pages.ProductsPage exposing (showProductsPage)
import Products exposing (getProducts, printProducts)
import Result exposing (andThen)

import String exposing (dropLeft, fromInt)
import Validation.ProductsValidation exposing (checkDataFieldInputP)
import Xml.Decode exposing (Decoder, float, int, list, node, requiredPath, run, single, string, succeed)
import XmlParser exposing (..)

import Types exposing (..)
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> (Model, Cmd Msg)
init _ =
  ( MainPage
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      -- Page navigation
      Go ToMainPage -> (MainPage, Cmd.none)
      Go ToProductsPage -> (ProductsPage Show Nothing, httpProducts)
      Go ToOrganizationsPage -> (OrganizationsPage Show Nothing, httpOrganizations)

      HttpAction (HttpGetProducts result) ->
          let
              productsList = case result of
                  Ok fullText -> getProducts fullText
                  Err e -> Err <| toString e
              products = case productsList of
                  Ok prds -> Just <| Prds prds
                  Err e -> Nothing
          in
              (ProductsPage Show products, Cmd.none)

      HttpAction (HttpGetOrganizations result) ->
          let
              organizationsList = case result of
                  Ok fullText -> getOrganizations fullText
                  Err e -> Err <| toString e

              organizations = case organizationsList of
                  Ok orgs -> Just <| Orgs orgs
                  Err e -> Nothing
          in
              (OrganizationsPage Show organizations, Cmd.none)

      HttpAction (HttpResult result) -> (MainPage, Cmd.none)

      PageAction Add Start -> (ProductsPage Add Nothing, Cmd.none)
      PageAction Add (Store productInput) -> (ProductsPage Add (Just productInput), Cmd.none)
      PageAction Add (Check productInput) -> case checkDataFieldInputP productInput of
          Ok (PrdInp productInput_) ->
              let
                  name = withDefault "default" productInput_.name
                  x = "0"
                  y = "0.5"
                  price = "88"
                  unitofmeasure = "pcs"
                  manufacturer = "1"
                  data =
                      [ header "name" name
                      , header "x" x
                      , header "y" y
                      , header "price" price
                      , header "unitofmeasure" unitofmeasure
                      , header "manufacturer" manufacturer
                      ]
              in
                  (ProductsPage Add Nothing, httpAddProduct data)

          Ok (OrgInp organizationInput_) -> Debug.todo "can not be here!"

          Err error -> (ProductsPage Add Nothing, Cmd.none)

      PageAction _ _ -> Debug.todo "пейдж акшон"



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  case model of
      MainPage -> showMainPage
      ProductsPage operation data -> showProductsPage operation data
      OrganizationsPage operation data -> showOrganizationsPage operation data
