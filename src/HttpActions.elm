module HttpActions exposing (..)


import Http
import Types exposing (HttpMsg(..), Msg(..))

urlBase = "http://58d878825f54.ngrok.io:8080/lab1coa-1.0-SNAPSHOT/"

httpProducts = Http.get
    { url = urlBase ++ "products"
    , expect = Http.expectString (\res -> HttpAction <| HttpGetProducts res)
    }

httpOrganizations = Http.get
    { url = urlBase ++ "organizations"
    , expect = Http.expectString (\res -> HttpAction <| HttpGetOrganizations res)
    }
