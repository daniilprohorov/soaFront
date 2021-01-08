module HttpActions exposing (..)


import Html as Encode
import Http
import Types exposing (HttpMsg(..), Msg(..))
import Utils exposing (formUrlencoded)
import Xml.Decode as Encode

urlBase = "http://localhost:8080/lab1coa-1.0-SNAPSHOT/"

parameters sort filterLIst itemsperpage page = case (sort, filterLIst) of
    (Nothing, Nothing) ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page) ]

    (Just s, Nothing) ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page)
        , ("sortBy", s)
        ]
    (Nothing, Just f) ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page)
        ] ++ f

    (Just s, Just f) ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page)
        , ("sortBy", s)
        ] ++ f

httpProducts sort filterList itemsperpage page =
    let
        params = parameters sort filterList itemsperpage page
    in
        Http.get
            { url = urlBase ++ "products/" ++ formUrlencoded params
            , expect = Http.expectString (\res -> HttpAction <| HttpGetProducts res)
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
