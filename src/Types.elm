module Types exposing (..)

import DataField exposing (DataField)
import Http

import Organizations exposing (Organization)
import Products exposing (Product)

type Model
  = MainPage
  | ProductsPage (Result String (List Product)) Operation (Maybe DataField)
  | OrganizationsPage (Result String (List Organization)) Operation (Maybe DataField)

type Msg = Go ToPage | PageAction Operation ActionType | HttpAction HttpMsg

type ToPage = ToMainPage | ToProductsPage | ToOrganizationsPage

type Operation = Edit | Delete | Add | Filter | Sort | Show

type ActionType = Start | Store DataField | Check DataField | Send DataField

type HttpMsg =
    HttpGetProducts (Result Http.Error String) |
    HttpGetOrganizations (Result Http.Error String)


