module Types exposing (..)

import Http

import Organizations exposing (Organization)
import Products exposing (Product)

type Model
  = MainPage
  | ProductsPage (Result String (List Product))
  | OrganizationsPage (Result String (List Organization))

type Msg
  = ToMainPage
  | ToProductsPage
  | ToOrganizationsPage
  | HttpGetProducts (Result Http.Error String)
  | HttpGetOrganizations (Result Http.Error String)
