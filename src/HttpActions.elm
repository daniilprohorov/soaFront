module HttpActions exposing (..)


import Html as Encode
import Http
import List exposing (filter, map)
import Maybe exposing (andThen, withDefault)
import Types exposing (HttpMsg(..), Msg(..))
import Utils exposing (formUrlencoded, isJust)
import Xml.Decode as Encode

urlBase = "http://localhost:8080/lab1coa-1.0-SNAPSHOT/"


parameters sort itemsperpage page = case sort of
    Nothing ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page) ]

    Just s ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page)
        , ("sortBy", s)
        ]
    --(Nothing, Just f) ->
    --    [ ("itemsperpage", String.fromInt itemsperpage)
    --    , ("page", String.fromInt page)
    --    ] ++ filtersToTuples f
    --
    --(Just s, Just f) ->
    --    [ ("itemsperpage", String.fromInt itemsperpage)
    --    , ("page", String.fromInt page)
    --    , ("sortBy", s)
    --    ] ++ filtersToTuples f

httpProducts sort filters filterApply elemsperpage page =
    let
        params = parameters sort elemsperpage page
        filtersStr = if filterApply then filters else Nothing
    in
        Http.get
            { url = urlBase ++ "products?" ++ formUrlencoded params ++ withDefault "" filters
            , expect = Http.expectString (\res -> HttpAction <| HttpGetProducts res filters filterApply elemsperpage page)
            }

httpOrganizations = Http.get
    { url = urlBase ++ "organizations"
    , expect = Http.expectString (\res -> HttpAction <| HttpGetOrganizations res)
    }

httpAddProduct data =
        Http.request
            { method = "POST"
            , headers = []
            , url = urlBase ++ "products"
            , body = data
            , expect = Http.expectString  (\res -> HttpAction <| HttpAddProduct res)
            , timeout = Nothing
            , tracker = Nothing
            }

httpDeleteProduct id =
        Http.request
            { method = "DELETE"
            , headers = []
            , url = urlBase ++ "products/" ++ String.fromInt id
            , body = Http.emptyBody
            , expect = Http.expectString  (\res -> HttpAction <| HttpDeleteProduct res)
            , timeout = Nothing
            , tracker = Nothing
            }

httpEditProduct id data =
        Http.request
            { method = "PUT"
            , headers = []
            , url = urlBase ++ "products/" ++ String.fromInt id
            , body = data
            , expect = Http.expectString  (\res -> HttpAction <| HttpEditProduct res)
            , timeout = Nothing
            , tracker = Nothing
            }

httpShowProduct id =
        Http.request
            { method = "GET"
            , headers = []
            , url = urlBase ++ "products/" ++ String.fromInt id
            , body = Http.emptyBody
            , expect = Http.expectString  (\res -> HttpAction <| HttpShowProduct res)
            , timeout = Nothing
            , tracker = Nothing
            }
