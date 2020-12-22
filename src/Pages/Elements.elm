module Pages.Elements exposing (..)

import DataField exposing (DataField(..), DataFieldInput(..))
import Element exposing (centerX, row, text)
import Element.Input as Input
import Types exposing (ActionType(..), Msg(..), Operation(..))
import Validation.Fields exposing (validateName)

updateName product str = case validateName str of
    Ok name -> PageAction Add (Store <| PrdInp {product | name = Just name})
    Err e   -> PageAction Add (Fail e)


inputProducts productInput = case productInput of
    PrdInp product ->
        row [ centerX ]
            [ Input.text []
                { onChange=updateName product
                , text = "Name"
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Name")
                }
            --, Input.text []
            --    { onChange=(\x_ -> PageAction Add (Store <| Prd {product | name = x_} ))
            --    , text = "x"
            --    , placeholder = Nothing
            --    , label = Input.labelAbove [] (text "Name")
            --    }
            ]

    _ -> Debug.todo "Can not go to this way"

