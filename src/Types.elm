module Types exposing (..)

import DataField exposing (DataField, DataFieldInput(..))
import Http

import Organizations exposing (Organization, organizationInputDef)
import Products exposing (Product, ProductInput, productInputDef)

type Model
  = MainPage
  | ProductsPage Operation (Maybe DataField)
  | OrganizationsPage Operation (Maybe DataField)

type Msg = Go ToPage | PageAction Operation | HttpAction HttpMsg

type ToPage = ToMainPage | ToProductsPage | ToOrganizationsPage

type Operation
    = Main (Maybe String) (Maybe String) Bool Int Int -- sort filter filterApply itemsperpage page
    | ShowById Int Bool (Maybe String) (Maybe String) -- id send error data
    | DeleteById Int Bool (Maybe String)
    | Add DataFieldInput Bool (Maybe String)
    | Edit Int DataFieldInput Bool (Maybe String)
    | UpdateFilter (Maybe String) (Maybe String) Bool Int Int (Maybe DataField) -- sort filter filterApply itemsperpage page data
--type ActionType = Start | Store DataFieldInput | Check DataFieldInput | Fail String | Send DataFieldInput

type HttpMsg
    = HttpGetProducts (Result Http.Error String) (Maybe String) Bool Int Int -- Data, filter, filterApply, Count Per page, page
    | HttpGetOrganizations (Result Http.Error String)
    | HttpAddProduct (Result Http.Error String)
    | HttpDeleteProduct (Result Http.Error String)
    | HttpShowProduct (Result Http.Error String)
    | HttpEditProduct (Result Http.Error String)


addProductMsgDef = PageAction (Add (PrdInp productInputDef) False Nothing)

addOrganizationMsgDef = PageAction (Add (OrgInp organizationInputDef) False Nothing)

defMain = Main Nothing Nothing False 20 1
