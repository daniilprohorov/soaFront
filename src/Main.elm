module Main exposing (main)

import DataField exposing (DataField(..))
import Debug exposing (toString)
import Element exposing (alignRight, centerX, centerY, column, el, fill, layout, padding, paragraph, rgb255, row, spacing, spacingXY, table, text, width)
import Element.Border as Border
import Element.Font exposing (center, size)
import Element.Input as Input exposing (button)
import Html.Attributes exposing (class)
import Http
import Browser
import Html exposing (Html, div, tbody, td, th, thead, tr)
import Html.Events exposing (onClick)
import HttpActions exposing (httpOrganizations, httpProducts)
import List exposing (all, map)
import Organizations exposing (getOrganizations, printOrganizations)
import Pages.MainPage exposing (showMainPage)
import Pages.OrganizationsPage exposing (showOrganizationsPage)
import Pages.ProductsPage exposing (showProductsPage)
import Products exposing (getProducts, printProducts)
import Result exposing (andThen)

import String exposing (dropLeft, fromInt)
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

      PageAction Add Start -> (ProductsPage Add Nothing, Cmd.none)
      PageAction operation actionType -> Debug.todo "надо бы сделать, да"



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  case model of
      MainPage -> showMainPage
      ProductsPage operation data -> showProductsPage operation data
      OrganizationsPage operation data -> showOrganizationsPage operation data
