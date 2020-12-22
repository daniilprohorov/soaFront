module Validation.ProductsValidation exposing (..)

import DataField exposing (DataFieldInput(..))
import Utils exposing (all, isJust)

checkDataFieldInputP dataFieldInput = case dataFieldInput of
    PrdInp products ->
        let
            nameInp = products.name
            xInp = products.x
            yInp = products.y
            creationdateInp = products.creationdate
            priceInp = products.price
            unitofmeasureInp = products.unitofmeasure
            manufacturerInp = products.manufacturer

            inpList =
                [ nameInp
                , xInp
                , yInp
                , creationdateInp
                , priceInp
                , unitofmeasureInp
                , manufacturerInp
                ]
        in
            if all isJust inpList then
                Ok dataFieldInput
            else
                Err "input data is not valid"

    _ -> Err "Data field input is not contain product type"

