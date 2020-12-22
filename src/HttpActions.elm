module HttpActions exposing (..)


import Http
import Types exposing (HttpMsg(..), Msg(..))

urlBase = "http://localhost:8080/lab1coa-1.0-SNAPSHOT/"

httpProducts = Http.get
    { url = urlBase ++ "products"
    , expect = Http.expectString (\res -> HttpAction <| HttpGetProducts res)
    }

httpOrganizations = Http.get
    { url = urlBase ++ "organizations"
    , expect = Http.expectString (\res -> HttpAction <| HttpGetOrganizations res)
    }

httpAddProduct data =
    Http.request
        { method = "POST"
        , headers = data
        , url = urlBase ++ "product"
        , body = Http.emptyBody
        , expect = Http.expectString  (\res -> HttpAction <| HttpResult res)
        , timeout = Nothing
        , tracker = Nothing
        }
