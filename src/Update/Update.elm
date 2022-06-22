module Update.Update exposing (..)

import Types exposing (Model(..), Msg(..))
import Update.HttpListener exposing (httpUpdate)
import Update.MainPageListener exposing (mainPageUpdate)
import Update.MovePageListener exposing (moveUpdate)
import Update.VehiclePageListener exposing (productPageUpdate)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Go page -> moveUpdate page
    PageAction operation -> pageAction model operation
    HttpAction httpMsg -> httpUpdate httpMsg model

pageAction model operation = case model of
    VehiclesPage _ _ -> productPageUpdate operation
    MainPage s a -> mainPageUpdate s a


