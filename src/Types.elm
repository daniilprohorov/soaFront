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
    = Main (Maybe String) (Maybe (List String)) Int Int -- sort [filter] itemsperpage page
    | ShowById Int
    | DeleteById Int Bool (Maybe String)
    | Add DataFieldInput Bool (Maybe String)
    | Edit Int DataFieldInput Bool (Maybe String)
    | IncPageCount
    | DecPageCount
    | IncCurrentPage
    | DecCurrentPage

--type ActionType = Start | Store DataFieldInput | Check DataFieldInput | Fail String | Send DataFieldInput

type HttpMsg
    = HttpGetProducts (Result Http.Error String)
    | HttpGetOrganizations (Result Http.Error String)
    | HttpAddProduct (Result Http.Error String)
    | HttpDeleteProduct (Result Http.Error String)
    | HttpEditProduct (Result Http.Error String)


addProductMsgDef = PageAction (Add (PrdInp productInputDef) False Nothing)

addOrganizationMsgDef = PageAction (Add (OrgInp organizationInputDef) False Nothing)

defMain = Main Nothing Nothing 20 1
