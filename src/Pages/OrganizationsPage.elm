module Pages.OrganizationsPage exposing (..)

import DataField exposing (DataField(..))
import Element exposing (centerX, paragraph, text)
import Element.Font exposing (center)
import Element.Input as Input exposing (button)
import Organizations exposing (printOrganizations)
import Pages.Layouts exposing (centerXLayout)
import Pages.Styles exposing (buttonStyle)
import Types exposing (Msg(..), Operation(..), ToPage(..))

--showOrganizationsPage operation data = case operation of
--    Show -> showOrganizations data
--    Add -> addOrganization data
--    _ -> Debug.todo "lul"
--
--showOrganizations data = case data of
--    Just (Orgs organizations) -> centerXLayout
--        [ paragraph [ center ] [ text "Organizations page" ]
--        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
--        , paragraph [centerX] [ printOrganizations organizations ]
--        , Input.text []
--            { onChange=(\s -> Go ToMainPage)
--            , text = "Name"
--            , placeholder = Nothing
--            , label = Input.labelAbove [] (text "Label")
--            }
--        ]
--
--    Just _ -> Debug.todo "show organizations error"
--
--    Nothing -> centerXLayout
--        [ paragraph [ center ] [ text "Organizations page" ]
--        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
--        , paragraph [centerX] [ text "wait" ]
--        ]
--
--addOrganization data = case data of
--    Just (Org organizations) -> centerXLayout
--        [ paragraph [ center ] [ text "Organizations page" ]
--        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
--        ]
--    Just _ -> Debug.todo "add organization error"
--
--    Nothing -> centerXLayout
--        [ paragraph [ center ] [ text "Organizations page" ]
--        , button buttonStyle {onPress=Just <| Go ToMainPage, label=text "Main page"}
--        , paragraph [centerX] [ text "wait" ]
--        ]
