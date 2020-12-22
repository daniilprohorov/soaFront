module Types exposing (..)

import DataField exposing (DataField, DataFieldInput)
import Http

import Organizations exposing (Organization)
import Products exposing (Product)

type Model
  = MainPage
  | ProductsPage Operation (Maybe DataField)
  | OrganizationsPage Operation (Maybe DataField)

type Msg = Go ToPage | PageAction Operation ActionType | HttpAction HttpMsg

type ToPage = ToMainPage | ToProductsPage | ToOrganizationsPage

type Operation = Edit | Delete | Add | Filter | Sort | Show

type ActionType = Start | Store DataFieldInput | Check DataFieldInput | Fail String | Send DataFieldInput

type HttpMsg =
    HttpGetProducts (Result Http.Error String) |
    HttpGetOrganizations (Result Http.Error String) |
    HttpResult (Result Http.Error String)


