module DataField exposing (..)


import Organizations exposing (Organization, OrganizationInput)
import Vehicles exposing (Vehicle, VehicleInput, Vehicles)

type DataField =  Org Organization | Prd Vehicle | Orgs (List Organization) | Prds Vehicles
type DataFieldInput =  VehInp VehicleInput
