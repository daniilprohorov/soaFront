module Update.MovePageListener exposing (..)

import HttpActions exposing (httpPriceSum, httpVehicles)
import Types exposing (Model(..), Operation(..), ToPage(..), defMain)

moveUpdate toPage = case toPage of
    ToMainPage -> (MainPage Nothing Nothing, httpPriceSum)
    ToVehiclesPage -> (VehiclesPage defMain Nothing, httpVehicles Nothing Nothing False 20 1)
