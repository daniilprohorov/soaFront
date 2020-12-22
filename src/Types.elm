module Types exposing (..)

import DataField exposing (DataField, DataFieldInput(..))
import Http

import Organizations exposing (Organization, organizationInputDef)
import Products exposing (Product, productInputDef)

type Model
  = MainPage
  | ProductsPage Operation (Maybe DataField)
  | OrganizationsPage Operation (Maybe DataField)

type Msg = Go ToPage | PageAction Operation | HttpAction HttpMsg

type ToPage = ToMainPage | ToProductsPage | ToOrganizationsPage

type Operation
    = Show
    | ShowById Int
    | DeleteById Int
    | Filter DataFieldInput
    | Sort String
    | Add DataFieldInput Bool (Maybe String)

type ActionType = Start | Store DataFieldInput | Check DataFieldInput | Fail String | Send DataFieldInput

type HttpMsg =
    HttpGetProducts (Result Http.Error String) |
    HttpGetOrganizations (Result Http.Error String) |
    HttpResult (Result Http.Error String)


addProductMsgDef = PageAction (Add (PrdInp productInputDef) False Nothing)

addOrganizationMsgDef = PageAction (Add (OrgInp organizationInputDef) False Nothing)


