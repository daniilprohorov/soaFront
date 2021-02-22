module Update.MainPageListener exposing (..)

import HttpActions exposing (httpPriceSum)
import Types exposing (Model(..))

mainPageUpdate s a = case (s, a) of
    (Nothing, _ )-> (MainPage s a, httpPriceSum)
    (_, _ )-> (MainPage s a, httpPriceSum)
