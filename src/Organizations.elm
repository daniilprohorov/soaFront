module Organizations exposing (..)

import Debug exposing (toString)
import Element exposing (fill, table, text)
import List exposing (map)
import Utils exposing (nodeToXmlString, traverse)
import Xml.Decode exposing (Decoder, int, list, node, requiredPath, run, single, string, succeed)
type alias Organization =
    { id : Int
    , name : String
    , fullname : String
    , employeescount : Int
    }
type alias Organizations =
    { organizations : List String
    }

organizationsDecoder : Decoder Organizations
organizationsDecoder =
    (requiredPath ["organization"] (list node)) (succeed (\v -> Organizations <| map nodeToXmlString v))


organizationDecoder : Decoder Organization
organizationDecoder =
    succeed Organization
        |> requiredPath [ "id" ] (single int)
        |> requiredPath [ "name" ] (single string)
        |> requiredPath [ "fullname" ] (single string)
        |> requiredPath [ "employeescount" ] (single int)

printOrganizations organizations = table []
    { data = organizations
    , columns =
        [
            { header = text "id"
            , width = fill
            , view = \organization -> text <| toString organization.id
            }
        ,
            { header = text "name"
            , width = fill
            , view = \organization -> text organization.name
            }
        ,
            { header = text "fullname"
            , width = fill
            , view = \organization -> text organization.fullname
            }
        ,
            { header = text "employeescount"
            , width = fill
            , view = \organization -> text <| toString organization.employeescount
            }
        ]
    }


getOrganizations : String -> Result String (List Organization)
getOrganizations str =
  let
      organizationsList = run organizationsDecoder str
  in
      case organizationsList of
          Err msg -> Err msg
          Ok listOfProducts ->
              let
                  listOfProduct : Result String (List Organization)
                  listOfProduct = traverse (run organizationDecoder) (listOfProducts.organizations)
              in
                  listOfProduct
