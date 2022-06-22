module Vehicles exposing (..)

import Debug exposing (toString)
import Element exposing (centerX, clip, fill, padding, paragraph, px, spacing, table, text)
import Element.Border as Border
import Element.Font as Font exposing (bold, size)
import Json.Decode exposing (Decoder, Error, decodeString, errorToString, field, float, int, list, map8, string, succeed)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import List exposing (map)
--import Types exposing (Msg(..))

import Http
--import Xml.Decode exposing (Decoder, float, int, list, node, requiredPath, run, single, string, succeed)

import Utils exposing (nodeToXmlString, traverse)
import Xml.Decode exposing (run)

--type alias vehicleInput =
--    { name : Maybe String
--    , x : Maybe String
--    , y : Maybe String
--    , creationdate : Maybe String
--    , price : Maybe String
--    , unitofmeasure : Maybe String
--    , manufacturer : Maybe String
--    }

type alias VehicleInput =
    { name: Maybe String
    , coordinate_x: Maybe String
    , coordinate_y: Maybe String
    , engine_power: Maybe String
    , distance_travelled: Maybe String
    , vehicle_type: Maybe String
    , fuel_type: Maybe String
    }

--vehicleInputDef = vehicleInput Nothing Nothing Nothing Nothing Nothing Nothing Nothing
vehicleInputDef = VehicleInput Nothing Nothing Nothing Nothing Nothing Nothing Nothing



--type alias Product =
--    { id : Int
--    , name : String
--    , x : Int
--    , y : Float
--    , creationdate : String
--    , price : Int
--    , unitofmeasure : String
--    , manufacturer : Int
--    }
--type alias vehicles =
--    { vehicles : List String
--    }

type alias Vehicle =
    { id: Int
    , name: String
    , coordinate_x: Float
    , coordinate_y: Int
    , engine_power: Int
    , distance_travelled: Float
    , creation_date: String
    , vehicle_type: String
    , fuel_type: String
    }
type alias Vehicles = List Vehicle
  --decode.succeed user
  --  |> required "id" int
  --  |> required "email" (nullable string) -- `null` decodes to `nothing`
  --  |> optional "name" string "(fallback if name is `null` or not present)"
  --  |> hardcoded 1.0

vehicleDecoder : Decoder Vehicle
vehicleDecoder =
  succeed Vehicle
    |> required "id" int
    |> required "name" string
    |> required "coordinate_x" float
    |> required "coordinate_y" int
    |> required "engine_power" int
    |> required "distance_travelled" float
    |> required "creation_date" string
    |> required "vehicle_type" string
    |> required "fuel_type" string

vehiclesDecoder : Decoder Vehicles
vehiclesDecoder = list vehicleDecoder

--vehiclesDecoder : Decoder vehicles
--vehiclesDecoder =
--    (requiredPath ["product"] (list node)) (succeed (\v -> vehicles <| map nodeToXmlString v))
--
--
--productDecoder : Decoder Product
--productDecoder =
--    succeed Product
--        |> requiredPath [ "id" ] (single int)
--        |> requiredPath [ "name" ] (single string)
--        |> requiredPath [ "x" ] (single int)
--        |> requiredPath [ "y" ] (single float)
--        |> requiredPath [ "creationdate" ] (single string)
--        |> requiredPath [ "price" ] (single int)
--        |> requiredPath [ "unitofmeasure" ] (single string)
--        |> requiredPath [ "manufacturer" ] (single int)

headerStyle = [bold, size 14]

--printvehicles vehicles = table [size 12, Font.center]
--    { data = vehicles
--    , columns =
--        [
--            { header = paragraph headerStyle [ text "id" ]
--            , width = fill
--            , view = \product -> text <| toString product.id
--            }
--        ,
--            { header = paragraph headerStyle [ text "name" ]
--            , width = fill
--            , view = \product -> text product.name
--            }
--        ,
--            { header = paragraph headerStyle [text "x" ]
--            , width = fill
--            , view = \product -> text <| toString product.x
--            }
--        ,
--            { header = paragraph headerStyle [ text "y" ]
--            , width = fill
--            , view = \product -> text <| toString product.y
--            }
--        ,
--            { header = paragraph headerStyle [ text "creationdate" ]
--            , width = fill
--            , view = \product -> text product.creationdate
--            }
--        ,
--            { header = paragraph headerStyle [ text "price"]
--            , width = fill
--            , view = \product -> text <| toString product.price
--            }
--        ,
--            { header = paragraph headerStyle [ text "unit\n", text "of\n", text "measure" ]
--            , width = fill
--            , view = \product -> text product.unitofmeasure
--            }
--        ,
--            { header = paragraph headerStyle [ text "manufacturer" ]
--            , width = fill
--            , view = \product -> text <| toString product.manufacturer
--            }
--        ]
--    }

printVehicles vehicles = table [size 12, Font.center]
    { data = vehicles
    , columns =
        [
            { header = paragraph headerStyle [ text "id" ]
            , width = fill
            , view = \v -> text <| toString v.id
            }
        ,
            { header = paragraph headerStyle [ text "name" ]
            , width = fill
            , view = \v -> text v.name
            }
        --,
        --    { header = paragraph headerStyle [text "x" ]
        --    , width = fill
        --    , view = \product -> text <| toString product.x
        --    }
        --,
        --    { header = paragraph headerStyle [ text "y" ]
        --    , width = fill
        --    , view = \product -> text <| toString product.y
        --    }
        --,
        --    { header = paragraph headerStyle [ text "creationdate" ]
        --    , width = fill
        --    , view = \product -> text product.creationdate
        --    }
        --,
        --    { header = paragraph headerStyle [ text "price"]
        --    , width = fill
        --    , view = \product -> text <| toString product.price
        --    }
        --,
        --    { header = paragraph headerStyle [ text "unit\n", text "of\n", text "measure" ]
        --    , width = fill
        --    , view = \product -> text product.unitofmeasure
        --    }
        --,
        --    { header = paragraph headerStyle [ text "manufacturer" ]
        --    , width = fill
        --    , view = \product -> text <| toString product.manufacturer
        --    }
        ]
    }

--getProduct str = run productDecoder str
--
--getvehicles : String -> Result String (List Product)
--getvehicles str =
--  let
--      vehiclesList = run vehiclesDecoder str
--  in
--      case vehiclesList of
--          Err msg -> Err msg
--          Ok listOfvehicles ->
--              let
--                  listOfProduct : Result String (List Product)
--                  listOfProduct = traverse (run productDecoder) (listOfvehicles.vehicles)
--              in
--                  listOfProduct

getVehicles : String -> Result String Vehicles
getVehicles str =  case decodeString vehiclesDecoder str of
          Err err -> Err <| errorToString err
          Ok v -> Ok v


getVehicle : String -> Result String Vehicle
getVehicle str = case decodeString vehicleDecoder str of
          Err err -> Err <| errorToString err
          Ok v -> Ok v
