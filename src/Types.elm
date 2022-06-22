module Types exposing (..)

import DataField exposing (DataField, DataFieldInput(..))
import Http

import Organizations exposing (Organization, organizationInputDef)
import Vehicles exposing (Vehicle, VehicleInput, vehicleInputDef)

type Model
  = MainPage (Maybe String) (Maybe String)
  | VehiclesPage Operation (Maybe DataField)

type Msg = Go ToPage | PageAction Operation | HttpAction HttpMsg

type ToPage = ToMainPage | ToVehiclesPage

type Operation
    = Main (Maybe String) (Maybe String) Bool Int Int -- sort filter filterApply itemsperpage page
    | ShowById Int Bool (Maybe String) (Maybe String) -- id send error data
    | DeleteById Int Bool (Maybe String)
    | Add DataFieldInput Bool (Maybe String)
    | Edit Int DataFieldInput Bool (Maybe String)
    | UpdateFilter (Maybe String) (Maybe String) Bool Bool Int Int (Maybe DataField) -- sort filter filterApply startWith itemsperpage page data
--type ActionType = Start | Store DataFieldInput | Check DataFieldInput | Fail String | Send DataFieldInput

type HttpMsg
    = HttpGetVehicles (Result Http.Error String) (Maybe String) Bool Int Int -- Data, filter, filterApply, Count Per page, page
    | HttpAddVehicle (Result Http.Error String)
    | HttpDeleteVehicle (Result Http.Error String)
    | HttpShowVehicle (Result Http.Error String)
    | HttpEditVehicle (Result Http.Error String)
    | HttpPriceSum (Result Http.Error String)
    | HttpPriceAvg (Result Http.Error String)


addProductMsgDef = PageAction (Add (VehInp vehicleInputDef) False Nothing)

defMain = Main Nothing Nothing False 20 1
