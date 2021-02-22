module Update.MovePageListener exposing (..)

import HttpActions exposing (httpOrganizations, httpPriceSum, httpProducts)
import Types exposing (Model(..), Operation(..), ToPage(..), defMain)

moveUpdate toPage = case toPage of
    ToMainPage -> (MainPage Nothing Nothing, httpPriceSum)
    ToProductsPage -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing False 20 1)
    ToOrganizationsPage -> (OrganizationsPage defMain Nothing, httpOrganizations Nothing Nothing False 20 1)
