module DataField exposing (..)


import Organizations exposing (Organization)
import Products exposing (Product)

type DataField =  Org Organization | Prd Product | Orgs (List Organization) | Prds (List Product)