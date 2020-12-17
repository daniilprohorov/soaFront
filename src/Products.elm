module Products exposing (..)

import Debug exposing (toString)
import Element exposing (fill, table, text)
import List exposing (map)
--import Types exposing (Msg(..))

import Http
import Xml.Decode exposing (Decoder, float, int, list, node, requiredPath, run, single, string, succeed)

import Utils exposing (nodeToXmlString, traverse)


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
