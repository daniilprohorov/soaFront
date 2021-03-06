module HttpActions exposing (..)


import Html as Encode
import Http
import List exposing (filter, map)
import Maybe exposing (andThen, withDefault)
import Types exposing (HttpMsg(..), Msg(..))
import Utils exposing (formUrlencoded, isJust)
import Xml.Decode as Encode

urlBase = "http://localhost:1099/lab1coa_war/"


parameters sort itemsperpage page = case sort of
    Nothing ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page) ]

    Just s ->
        [ ("itemsperpage", String.fromInt itemsperpage)
        , ("page", String.fromInt page)
        , ("sortBy", s)
        ]

httpProducts sort filters filterApply elemsperpage page =
    let
        params = parameters sort elemsperpage page
        filterStr = if filterApply
            then withDefault "" (andThen (\s -> Just <| "&"++s ) filters)
            else ""
    in
        Http.get
            { url = urlBase ++ "products?" ++ formUrlencoded params ++ filterStr
            , expect = Http.expectString (\res -> HttpAction <| HttpGetProducts res filters filterApply elemsperpage page)
            }

httpProductsP sort filters filterApply elemsperpage page =
    let
        filterStr = if filterApply
            then withDefault "" (andThen (\s -> Just <| "&"++s ) filters)
            else ""
    in
        Http.get
            { url = urlBase ++ "products-start-with?" ++ filterStr
            , expect = Http.expectString (\res -> HttpAction <| HttpGetProducts res filters filterApply elemsperpage page)
            }

httpOrganizations sort filters filterApply elemsperpage page =
    let
        params = parameters sort elemsperpage page
        filterStr = if filterApply
            then withDefault "" (andThen (\s -> Just <| "&"++s ) filters)
            else ""
    in
        Http.get
            { url = urlBase ++ "organizations?" ++ formUrlencoded params ++ filterStr
            , expect = Http.expectString (\res -> HttpAction <| HttpGetOrganizations res filters filterApply elemsperpage page)
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

httpAddOrganization data =
        Http.request
            { method = "POST"
            , headers = []
            , url = urlBase ++ "organizations"
            , body = data
            , expect = Http.expectString  (\res -> HttpAction <| HttpAddOrganization res)
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

httpDeleteOrganization id =
        Http.request
            { method = "DELETE"
            , headers = []
            , url = urlBase ++ "organizations/" ++ String.fromInt id
            , body = Http.emptyBody
            , expect = Http.expectString  (\res -> HttpAction <| HttpDeleteOrganization res)
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

httpEditOrganization id data =
        Http.request
            { method = "PUT"
            , headers = []
            , url = urlBase ++ "organizations/" ++ String.fromInt id
            , body = data
            , expect = Http.expectString  (\res -> HttpAction <| HttpEditOrganization res)
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

httpShowOrganization id =
        Http.request
            { method = "GET"
            , headers = []
            , url = urlBase ++ "organizations/" ++ String.fromInt id
            , body = Http.emptyBody
            , expect = Http.expectString  (\res -> HttpAction <| HttpShowOrganization res)
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
