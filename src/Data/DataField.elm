module DataField exposing (..)


import Organizations exposing (Organization, OrganizationInput)
import Products exposing (Product, ProductInput)

type DataField =  Org Organization | Prd Product | Orgs (List Organization) | Prds (List Product)
type DataFieldInput =  OrgInp OrganizationInput | PrdInp ProductInput
