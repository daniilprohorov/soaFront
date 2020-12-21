module Main exposing (main)

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
      Go ToProductsPage -> (ProductsPage (Err "Loading") Show Nothing, httpProducts)
      Go ToOrganizationsPage -> (OrganizationsPage (Err "Loading") Show Nothing, httpOrganizations)

      HttpAction (HttpGetProducts result) ->
          let
              productsList = case result of
                  Ok fullText -> getProducts fullText
                  Err e -> Err <| toString e
          in
              (ProductsPage productsList Show Nothing, Cmd.none)

      HttpAction (HttpGetOrganizations result) ->
          let
              organizationsList = case result of
                  Ok fullText -> getOrganizations fullText
                  Err e -> Err <| toString e
          in
              (OrganizationsPage organizationsList Show Nothing, Cmd.none)

      PageAction Add Start -> (ProductsPage (Err "Not") Add Nothing, Cmd.none)
      PageAction operation actionType -> Debug.todo "надо бы сделать, да"



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
                      , button buttonStyle {onPress=Just <| Go ToProductsPage, label=text "Products page"}
                      , button buttonStyle {onPress=Just <| Go ToOrganizationsPage, label=text "Organizations page"}
                      ]
                  ]

      ProductsPage products Show df->
          layout [] <|
              row [ centerX]
                  [   column [ centerX, spacing 10 ]
                      [ paragraph [ center ] [ text "Products page" ]
                      , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
                      , paragraph [centerX] [
                          case products of
                              Ok prds -> printProducts prds
                              Err e   -> text e
                          ]
                      ]
                  ]

      ProductsPage products Add (Just prdPrd) ->
          let
              product = (\(Prd productInside) -> productInside) prdPrd

          in
              layout [] <|
                  row [ centerX]
                      [   column [ centerX, spacing 10 ]
                          [ paragraph [ center ] [ text "Products page" ]
                          , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
                          , Input.text []
                              { onChange=(\s -> ProductsPage products (Just (Prd { product | name = "kek"})))
                              , text = "Name"
                              , placeholder = Nothing
                              , label = Input.labelAbove [] (text "Label")
                              }
                          ]
                      ]
      OrganizationsPage organizations Show df ->
          layout [] <|
              row [ Element.centerX]
                  [   column [ Element.centerX]
                      [ paragraph [center] [text "Organizations page"]
                      , button buttonStyle {onPress=Just <| PageAction Add Start, label=text "Add organization"}
                      , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
                      , paragraph [centerX] [
                          case organizations of
                              Ok orgs -> printOrganizations orgs
                              Err e   -> text e
                          ]
                      ]
                  ]
      ProductsPage _ Edit _ -> Debug.todo "kek"
      ProductsPage _ Delete _ -> Debug.todo "kek"
      ProductsPage _ Add _ -> Debug.todo "kek"
      ProductsPage _ Filter _ -> Debug.todo "kek"
      ProductsPage _ Sort _ -> Debug.todo "kek"

      OrganizationsPage _ Edit _ -> Debug.todo "kek"
      OrganizationsPage _ Delete _ -> Debug.todo "kek"
      OrganizationsPage _ Add _ -> Debug.todo "kek"
      OrganizationsPage _ Filter _ -> Debug.todo "kek"
      OrganizationsPage _ Sort _ -> Debug.todo "kek"
