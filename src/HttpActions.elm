module HttpActions exposing (..)


import Html as Encode
import Http
import List exposing (filter, map)
import Maybe exposing (andThen, withDefault)
import Types exposing (HttpMsg(..), Msg(..))
import Utils exposing (formUrlencoded, isJust)
import Xml.Decode as Encode

urlBase = "http://localhost:8080/api/"


parameters sort itemsperpage page = case sort of
    Nothing ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page) ]

    Just s ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page)
        , ("sortBy", s)
        ]

httpVehicles sort filters filterApply elemsperpage page =
    let
        params = parameters sort elemsperpage page
        filterStr = if filterApply
            then withDefault "" (andThen (\s -> Just <| "&"++s ) filters)
            else ""
    in
        Http.get
            { url = urlBase ++ "vehicles" -- ++ formUrlencoded params ++ filterStr
            , expect = Http.expectString (\res -> HttpAction <| HttpGetVehicles res filters filterApply elemsperpage page)
            }

httpVehiclesP sort filters filterApply elemsperpage page =
    let
        filterStr = if filterApply
            then withDefault "" (andThen (\s -> Just <| "&"++s ) filters)
            else ""
    in
        Http.get
            { url = urlBase ++ "products-start-with?" ++ filterStr
            , expect = Http.expectString (\res -> HttpAction <| HttpGetVehicles res filters filterApply elemsperpage page)
            }

httpAddVehicle data =
        Http.request
            { method = "POST"
            , headers = []
            , url = urlBase ++ "vehicles"
            , body = data
            , expect = Http.expectString  (\res -> HttpAction <| HttpAddVehicle res)
            , timeout = Nothing
            , tracker = Nothing
            }


httpDeleteVehicle id =
        Http.request
            { method = "DELETE"
            , headers = []
            , url = urlBase ++ "vehicles/" ++ String.fromInt id
            , body = Http.emptyBody
            , expect = Http.expectString  (\res -> HttpAction <| HttpDeleteVehicle res)
            , timeout = Nothing
            , tracker = Nothing
            }


httpEditVehicle id data =
        Http.request
            { method = "PUT"
            , headers = []
            , url = urlBase ++ "vehicles/" ++ String.fromInt id
            , body = data
            , expect = Http.expectString  (\res -> HttpAction <| HttpEditVehicle res)
            , timeout = Nothing
            , tracker = Nothing
            }

httpShowVehicle id =
        Http.request
            { method = "GET"
            , headers = []
            , url = urlBase ++ "vehicles/" ++ String.fromInt id
            , body = Http.emptyBody
            , expect = Http.expectString  (\res -> HttpAction <| HttpShowVehicle res)
            , timeout = Nothing
            , tracker = Nothing
            }


httpPriceSum  =
        Http.request
            { method = "GET"
            , headers = []
            , url = urlBase ++ "price-sum"
            , body = Http.emptyBody
            , expect = Http.expectString  (\res -> HttpAction <| HttpPriceSum res)
            , timeout = Nothing
            , tracker = Nothing
            }

httpPriceAvg  =
        Http.request
            { method = "GET"
            , headers = []
            , url = urlBase ++ "price-avg"
            , body = Http.emptyBody
            , expect = Http.expectString  (\res -> HttpAction <| HttpPriceAvg res)
            , timeout = Nothing
            , tracker = Nothing
            }
