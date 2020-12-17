module Main exposing (main)

import Debug exposing (toString)
import Element exposing (alignRight, centerX, column, fill, layout, padding, paragraph, rgb255, row, table, text, width)
import Element.Border as Border
import Html.Attributes exposing (class)
import Http
import Browser
import Html exposing (Html, div, tbody, td, th, thead, tr)
import Html.Events exposing (onClick)
import List exposing (all, map)
import Result exposing (andThen)
import String exposing (dropLeft, fromInt)
import Xml.Decode exposing (Decoder, float, int, list, node, requiredPath, run, single, string, succeed)
import XmlParser exposing (..)
import Element.Input exposing (button)

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
  = MainPage
  | ProductsPage (Result String (List Product))
  | OrganizationsPage String

init : () -> (Model, Cmd Msg)
init _ =
  ( MainPage
  , Cmd.none
  )

httpProducts = Http.get
    { url = "http://localhost:8080/lab1coa-1.0-SNAPSHOT/products"
    , expect = Http.expectString HttpGetProducts
    }

type alias Product =
    { id : Int
    , name : String
    , x : Int
    , y : Float
    , creationdate : String
    , price : Int
    , unitofmeasure : String
    , manufacturer : Int
    }
type alias Products =
    { products : List String
    }

productsDecoder : Decoder Products
productsDecoder =
    (requiredPath ["product"] (list node)) (succeed (\v -> Products <| map nodeToXmlString v))


productDecoder : Decoder Product
productDecoder =
    succeed Product
        |> requiredPath [ "id" ] (single int)
        |> requiredPath [ "name" ] (single string)
        |> requiredPath [ "x" ] (single int)
        |> requiredPath [ "y" ] (single float)
        |> requiredPath [ "creationdate" ] (single string)
        |> requiredPath [ "price" ] (single int)
        |> requiredPath [ "unitofmeasure" ] (single string)
        |> requiredPath [ "manufacturer" ] (single int)

type Msg
  = ToMainPage | ToProductsPage | ToOrganizationsPage | HttpGetProducts (Result Http.Error String)

printProducts products = table []
    { data = products
    , columns =
        [
            { header = text "id"
            , width = fill
            , view = \product -> text <| toString product.id
            }
        ,
            { header = text "name"
            , width = fill
            , view = \product -> text product.name
            }
        ,
            { header = text "x"
            , width = fill
            , view = \product -> text <| toString product.x
            }
        ,
            { header = text "y"
            , width = fill
            , view = \product -> text <| toString product.y
            }
        ,
            { header = text "creationdate"
            , width = fill
            , view = \product -> text product.creationdate
            }
        ,
            { header = text "price"
            , width = fill
            , view = \product -> text <| toString product.price
            }
        ,
            { header = text "unitofmeasure"
            , width = fill
            , view = \product -> text product.unitofmeasure
            }
        ,
            { header = text "manufacturer"
            , width = fill
            , view = \product -> text <| toString product.manufacturer
            }
        ]
    }


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

      ToProductsPage -> (ProductsPage (Err "Loading"), httpProducts)
      ToOrganizationsPage -> (OrganizationsPage "", Cmd.none)

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
              row [ Element.centerX]
                  [   column [ Element.centerX]
                      [ paragraph [centerX] [text "Main page"]
                      , button buttonStyle {onPress=Just ToProductsPage, label=text "Products page"}
                      , button buttonStyle {onPress=Just ToOrganizationsPage, label=text "Organizations page"}
                      ]
                  ]

      ProductsPage products ->
          layout [] <|
              row [ Element.centerX]
                  [   column [ Element.centerX]
                      [ paragraph [centerX] [text "Products page"]
                      , button buttonStyle {onPress=Just ToMainPage, label=text "Main page"}
                      --, paragraph [centerX] [text <| toString products]
                      , paragraph [centerX] [
                          case products of
                              Ok prds -> printProducts prds
                              Err e   -> text e
                          ]
                      ]
                  ]
      OrganizationsPage t ->
          layout [] <|
              row [ Element.centerX]
                  [   column [ Element.centerX]
                      [ paragraph [centerX, width fill] [text "Optimizations page"]
                      , button buttonStyle {onPress=Just ToMainPage, label=text "Main page"}
                      --, paragraph [centerX] [text <| toString products]
                      ]
                  ]

nodeToXmlString n = dropLeft 5 <| format <| Xml [ProcessingInstruction "" ""] Nothing n

isOk : Result a b -> Bool
isOk res =
    case res of
        Ok _ -> True
        _    -> False


getProducts : String -> Result String (List Product)
getProducts str =
  let
      productsList = run productsDecoder str
  in
      case productsList of
          Err msg -> Err msg
          Ok listOfProducts ->
              let
                  listOfProduct : Result String (List Product)
                  listOfProduct = traverse (run productDecoder) (listOfProducts.products)
              in
                  listOfProduct


traverse : (a -> Result String b) -> List a -> Result String (List b)
traverse f l =
    case l of
        []        -> Ok []
        (x :: xs) -> Result.map2 (::) (f x) <| traverse f xs