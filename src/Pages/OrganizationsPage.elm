module Pages.OrganizationsPage exposing (..)

import DataField exposing (DataField(..), DataFieldInput(..))
import Element exposing (centerX, column, paragraph, px, text, width)
import Element.Font exposing (center)
import Element.Input as Input exposing (button)
import Maybe exposing (withDefault)
import Organizations exposing (organizationInputDef, printOrganizations)
import Pages.Elements exposing (deleteEl, editOrganizationEl, inputOrganizationEl, showByIdEl)
import Pages.Layouts exposing (centerXLayout, twoColumnsLayout)
import Pages.Styles exposing (buttonStyle)
import Types exposing (Msg(..), Operation(..), ToPage(..), addOrganizationMsgDef, defMain)


showOrganizationsPage operation data = case operation of
    Main sort filter filterApply elemsPerPage page-> mainOrganizations data sort filter filterApply elemsPerPage page
    Add (OrgInp dataFieldInput) send fail -> addOrganization dataFieldInput send fail
    Edit id (OrgInp dataFieldInput) send fail -> editProduct id dataFieldInput send fail
    DeleteById id _ fail -> deleteProduct id fail
    ShowById id _ fail res -> showByIdProduct id fail data
    _ -> Debug.todo "lul"

mainOrganizations data sort filter filterApply elemsPerPage page = case data of
    Just (Orgs organizations) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Organizations" ]
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , column buttonStyle
            [ paragraph [center] [text "Sort"]
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "id") filter filterApply elemsPerPage page), label=text"id"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "name") filter filterApply elemsPerPage page), label=text"name"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "fullname") filter filterApply elemsPerPage page), label=text"fullname"}
            , button buttonStyle {onPress=Just <| PageAction (Main (Just "employeescount") filter filterApply elemsPerPage page), label=text"employeescount"}
            ]
        , column buttonStyle
            [ paragraph [center] [text "Filter"]
            , Input.text [width <| px 400]
                { onChange=(\s -> PageAction (UpdateFilter sort (Just s) False False elemsPerPage page data))
                , text = withDefault "" filter
                , placeholder = Nothing
                , label = Input.labelAbove [center] (text "FILTER VALUE")
                }
            , button buttonStyle {onPress=Just <|   PageAction (UpdateFilter sort filter True False elemsPerPage page data), label=text"APPLY"}
            ]
        , button buttonStyle {onPress=Just <| addOrganizationMsgDef, label=text "Add Organization"}
        , button buttonStyle {onPress=Just <| PageAction (DeleteById 0 False Nothing), label=text "Delete by Id"}
        , button buttonStyle {onPress=Just <| PageAction (Edit 0 (OrgInp organizationInputDef) False Nothing), label=text "Edit"}
        , button buttonStyle {onPress=Just <| PageAction (ShowById 0 False Nothing Nothing), label=text "Show by id"}
        , paragraph [center]
            [ text <| "Elems per page = " ++ (String.fromInt elemsPerPage)
            , button buttonStyle {onPress=Just <| PageAction (Main sort filter filterApply (elemsPerPage + 1) page), label=text"+"}
            , button buttonStyle {onPress=Just <| PageAction
                (if elemsPerPage >= 2
                    then Main sort filter filterApply (elemsPerPage - 1) page
                    else Main sort filter filterApply elemsPerPage page
                ), label=text"-"}
            ]

        , paragraph [center]
            [ text <| "Page = " ++ (String.fromInt page)
            , button buttonStyle {onPress=Just <| PageAction
                (if page >= 2
                    then Main sort filter filterApply elemsPerPage (page - 1)
                    else Main sort filter filterApply elemsPerPage page
                ), label=text"<"}
            , button buttonStyle {onPress=Just <| PageAction (Main sort filter filterApply elemsPerPage (page+1)), label=text">"}
            ]
        ]
        [ paragraph [centerX] [ printOrganizations organizations ]
        ]

    Just _ -> Debug.todo "show products error"

    Nothing -> twoColumnsLayout
        [ paragraph [ center ] [ text "Organization" ]
        , button buttonStyle {onPress=Just <| addOrganizationMsgDef, label=text "Add Organization"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        ]
        [ paragraph [centerX] [ text "wait" ]
        ]

addOrganization dataFieldInput send fail = case (dataFieldInput, send, fail) of
    (organizationInput, False, Nothing) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Organizations" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Organizations main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Add (OrgInp organizationInput) True Nothing), label=text "Send"}
        ]
        [ inputOrganizationEl organizationInput
        ]
    (organizationInput, _, Just msg) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Add Organizations" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Organizations main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Add (OrgInp organizationInput) True Nothing), label=text "Send"}
        ]
        [ inputOrganizationEl organizationInput
        , paragraph [centerX] [ text msg ]
        ]

    (_, True, Nothing) -> Debug.todo "Error in add product"

editProduct id dataFieldInput send fail = case (dataFieldInput, send, fail) of
    (organizationInput, False, Nothing) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Edit organization" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Organizations main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Edit id (OrgInp organizationInput) True Nothing), label=text "Send"}
        ]
        [ editOrganizationEl id organizationInput
        ]
    (organizationInput, _, Just msg) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Edit organizations" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Organizations main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (Edit id (OrgInp organizationInput) True Nothing), label=text "Send"}
        ]
        [ editOrganizationEl id organizationInput
        , paragraph [centerX] [ text msg ]
        ]

    (_, True, Nothing) -> Debug.todo "Error in add ogranization"

deleteProduct id fail = case fail of
    Nothing -> twoColumnsLayout
        [ paragraph [ center ] [ text "Delete Organization" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Organizations main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (DeleteById id True Nothing), label=text "Delete"}
        ]
        [ deleteEl id
        ]

    Just e -> twoColumnsLayout
        [ paragraph [ center ] [ text "Delete Organization" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Organization main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (DeleteById id True Nothing), label=text "Delete"}
        ]
        [ deleteEl id
        , paragraph [centerX] [ text e ]
        ]

showByIdProduct id fail data = case (fail, data) of
    (Nothing, Nothing) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Show organization by id" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Organizatoins main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (ShowById id True Nothing Nothing), label=text "Show"}
        ]
        [ showByIdEl id
        ]

    (Nothing, Just (Org organization)) -> twoColumnsLayout
        [ paragraph [ center ] [ text "Show product by id" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Products main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        ]

        [ paragraph [centerX] [ printOrganizations [organization] ]
        ]
    (Nothing, Just (Prd _)) -> Debug.todo "only organizations"
    (Nothing, Just (Prds _)) -> Debug.todo "only organizations"
    (Nothing, Just (Orgs _)) -> Debug.todo "only organizations"

    (Just e, _) -> twoColumnsLayout [ paragraph [ center ] [ text "Show organizatoin by id" ]
        , button buttonStyle {onPress=Just <| PageAction defMain , label=text "Organizations main"}
        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
        , button buttonStyle {onPress= Just <| PageAction (ShowById id True Nothing Nothing), label=text "Show"}
        ]
        [ showByIdEl id
        , paragraph [centerX] [ text e ]
        ]

