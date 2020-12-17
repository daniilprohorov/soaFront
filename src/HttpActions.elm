module HttpActions exposing (..)


import Http
import Types exposing (Msg(..))

urlBase = "http://60ada6e2b6e6.ngrok.io/lab1coa-1.0-SNAPSHOT/"

httpProducts = Http.get
    { url = urlBase ++ "products"
    , expect = Http.expectString HttpGetProducts
    }

httpOrganizations = Http.get
    { url = urlBase ++ "organizations"
    , expect = Http.expectString HttpGetOrganizations
    }
