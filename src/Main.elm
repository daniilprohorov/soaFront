module Main exposing (main)

import Debug exposing (toString)
import Http
--
--import Html exposing (text)
--
--main = text "kek"
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (all, map)
import Xml.Decode exposing (Decoder, float, int, list, requiredPath, run, single, string, succeed)

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type Model
  = Failure
  | Loading
  | Success String

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "http://localhost:8080/lab1coa-1.0-SNAPSHOT/products"
      , expect = Http.expectString GotText
      }
  )
-- UPDATE
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
    succeed Products
        |> requiredPath ["product"] (list string)

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
  = GotText (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      --div [] [ text <| toString <| run productsDecoder
      --    """
      --    <products>
      --        <product>kek</product>
      --        <product>lol</product>
      --    </products>
      --    """
      -- ]
      div [] [ text <| toString <| getProducts fullText ]
      --div [] [ text fullText ]

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
                  listOfProduct : List (Result String Product)
                  listOfProduct = map (run productDecoder) (listOfProducts.products)
              in
                  if all isOk listOfProduct
                  then Ok <| map
                      (\r ->
                          case r of
                              Ok p -> p
                              Err _ -> _
                      ) listOfProduct
                  else Err "Error to decode product"
      --map (run productDecoder) productsList
