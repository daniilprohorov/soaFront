module Update.MovePageListener exposing (..)

import HttpActions exposing (httpOrganizations, httpProducts)
import Types exposing (Model(..), Operation(..), ToPage(..), defMain)

moveUpdate toPage = case toPage of
    ToMainPage -> (MainPage, Cmd.none)
    ToProductsPage -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing False 20 1)
    ToOrganizationsPage -> (OrganizationsPage defMain Nothing, httpOrganizations)
