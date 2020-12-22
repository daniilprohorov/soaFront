module Pages.Elements exposing (..)

import DataField exposing (DataField(..), DataFieldInput(..))
import Element exposing (centerX, row, text)
import Element.Input as Input
import Types exposing (ActionType(..), Msg(..), Operation(..))
import Validation.Fields exposing (validateName)

updateName product str = case validateName str of
    Ok name -> PageAction <| Add (PrdInp {product | name = Just name}) False Nothing
    Err e   -> PageAction <| Add (PrdInp product) False (Just e)


inputProducts productInput =
    row [ centerX ]
        [ Input.text []
            { onChange=updateName productInput
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

