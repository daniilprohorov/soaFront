module Main exposing (main)

import Debug exposing (toString)
import Element exposing (alignRight, centerX, centerY, column, el, fill, layout, padding, paragraph, rgb255, row, spacing, spacingXY, table, text, width)
import Element.Border as Border
import Element.Font exposing (center, size)
import Html.Attributes exposing (class)
import Http
import Browser
import Html exposing (Html, div, tbody, td, th, thead, tr)
import Html.Events exposing (onClick)
import HttpActions exposing (httpOrganizations, httpProducts)
import List exposing (all, map)
import Organizations exposing (getOrganizations, printOrganizations)
import Products exposing (getProducts, printProducts)
import Result exposing (andThen)
import String exposing (dropLeft, fromInt)
import Xml.Decode exposing (Decoder, float, int, list, node, requiredPath, run, single, string, succeed)
import XmlParser exposing (..)
import Element.Input exposing (button)

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
      ToMainPage -> (MainPage, Cmd.none)
      HttpGetProducts result ->
          let
              productsList = case result of
                  Ok fullText -> getProducts fullText
                  Err e -> Err <| toString e
          in
              (ProductsPage productsList, Cmd.none)

      HttpGetOrganizations result ->
          let
              organizationsList = case result of
                  Ok fullText -> getOrganizations fullText
                  Err e -> Err <| toString e
          in
              (OrganizationsPage organizationsList, Cmd.none)

      ToProductsPage -> (ProductsPage (Err "Loading"), httpProducts)
      ToOrganizationsPage -> (OrganizationsPage (Err "Loading"), httpOrganizations)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

buttonStyle =
    [ padding 5
    , Border.width 1
    , Border.rounded 3
    , Border.color <| rgb255 200 200 200
    , centerX
    ]

view : Model -> Html Msg
view model =
  case model of
      MainPage ->
          layout [] <|
              row [ centerX, centerY ]
                  [   column [ centerX, spacing 10 ]
                      [ paragraph [center, size 50] [ text "Main page" ]
                      , button buttonStyle {onPress=Just ToProductsPage, label=text "Products page"}
                      , button buttonStyle {onPress=Just ToOrganizationsPage, label=text "Organizations page"}
                      ]
                  ]

      ProductsPage products ->
          layout [] <|
              row [ centerX]
                  [   column [ centerX, spacing 10 ]
                      [ paragraph [ center ] [ text "Products page" ]
                      , button buttonStyle {onPress=Just ToMainPage, label=text "Main page"}
                      , paragraph [centerX] [
                          case products of
                              Ok prds -> printProducts prds
                              Err e   -> text e
                          ]
                      ]
                  ]
      OrganizationsPage organizations ->
          layout [] <|
              row [ Element.centerX]
                  [   column [ Element.centerX]
                      [ paragraph [centerX, width fill] [text "Optimizations page"]
                      , button buttonStyle {onPress=Just ToMainPage, label=text "Main page"}
                      , paragraph [centerX] [
                          case organizations of
                              Ok orgs -> printOrganizations orgs
                              Err e   -> text e
                          ]
                      ]
                  ]

