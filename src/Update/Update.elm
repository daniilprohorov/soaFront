module Update.Update exposing (..)

import Types exposing (Model(..), Msg(..))
import Update.HttpListener exposing (httpUpdate)
import Update.MainPageListener exposing (mainPageUpdate)
import Update.MovePageListener exposing (moveUpdate)
import Update.OrganizationPageListener exposing (organizationPageUpdate)
import Update.ProductPageListener exposing (productPageUpdate)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Go page -> moveUpdate page
    PageAction operation -> pageAction model operation
    HttpAction httpMsg -> httpUpdate httpMsg

pageAction model operation = case model of
    ProductsPage _ _ -> productPageUpdate operation
    OrganizationsPage _ _ -> organizationPageUpdate operation
    MainPage -> mainPageUpdate


