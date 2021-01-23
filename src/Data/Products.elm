module Products exposing (..)

import Debug exposing (toString)
import Element exposing (centerX, clip, fill, padding, paragraph, px, spacing, table, text)
import Element.Border as Border
import Element.Font as Font exposing (bold, size)
import List exposing (map)
--import Types exposing (Msg(..))

import Http
import Xml.Decode exposing (Decoder, float, int, list, node, requiredPath, run, single, string, succeed)

import Utils exposing (nodeToXmlString, traverse)

type alias ProductInput =
    { name : Maybe String
    , x : Maybe String
    , y : Maybe String
    , creationdate : Maybe String
    , price : Maybe String
    , unitofmeasure : Maybe String
    , manufacturer : Maybe String
    }

productInputDef = ProductInput Nothing Nothing Nothing Nothing Nothing Nothing Nothing


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

headerStyle = [bold, size 14]

printProducts products = table [size 12, Font.center]
    { data = products
    , columns =
        [
            { header = paragraph headerStyle [ text "id" ]
            , width = fill
            , view = \product -> text <| toString product.id
            }
        ,
            { header = paragraph headerStyle [ text "name" ]
            , width = fill
            , view = \product -> text product.name
            }
        ,
            { header = paragraph headerStyle [text "x" ]
            , width = fill
            , view = \product -> text <| toString product.x
            }
        ,
            { header = paragraph headerStyle [ text "y" ]
            , width = fill
            , view = \product -> text <| toString product.y
            }
        ,
            { header = paragraph headerStyle [ text "creationdate" ]
            , width = fill
            , view = \product -> text product.creationdate
            }
        ,
            { header = paragraph headerStyle [ text "price"]
            , width = fill
            , view = \product -> text <| toString product.price
            }
        ,
            { header = paragraph headerStyle [ text "unit\n", text "of\n", text "measure" ]
            , width = fill
            , view = \product -> text product.unitofmeasure
            }
        ,
            { header = paragraph headerStyle [ text "manufacturer" ]
            , width = fill
            , view = \product -> text <| toString product.manufacturer
            }
        ]
    }

getProduct str = run productDecoder str

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
