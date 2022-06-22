module Organizations exposing (..)

import Debug exposing (toString)
import Element exposing (centerX, el, fill, padding, rgb255, spacing, table, text)
import Element.Border as Border
import Element.Input exposing (button)
import List exposing (map)
import Utils exposing (nodeToXmlString, traverse)
import Xml.Decode exposing (Decoder, int, list, node, requiredPath, run, single, string, succeed)
type alias Organization =
    { id : Int
    , name : String
    , fullname : String
    , employeescount : Int
    }


type alias OrganizationInput =
    { name : Maybe String
    , fullname : Maybe String
    , employeescount : Maybe String
    }
organizationInputDef = OrganizationInput Nothing Nothing Nothing

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

simpleBorders = [Border.width 1, Border.color <| rgb255 200 200 200]

printOrganizations organizations = table [spacing 10]
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


buttonStyle =
    [ padding 5
    , Border.width 1
    , Border.rounded 3
    , Border.color <| rgb255 200 200 200
    , centerX
    ]


getOrganization str = run organizationDecoder str

getOrganizations : String -> Result String (List Organization)
getOrganizations str =
  let
      organizationsList = run organizationsDecoder str
  in
      case organizationsList of
          Err msg -> Err msg
          Ok listOfvehicles ->
              let
                  listOfProduct : Result String (List Organization)
                  listOfProduct = traverse (run organizationDecoder) (listOfvehicles.organizations)
              in
                  listOfProduct
