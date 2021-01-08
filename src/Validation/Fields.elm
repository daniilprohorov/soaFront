module Validation.Fields exposing (..)
import List as L exposing (map)
import String as S exposing (contains, fromChar, split, toList)
import Utils exposing (and, isJust, isOk)

letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
digits = "0123456789"

validateName str = case str of
    "" -> Nothing
    s  -> if and <| map (\c -> contains (fromChar c) letters) <| toList s
        then Just s
        else Nothing

validateInt str = case str of
    "" -> Nothing
    s  -> if (and <| map (\c -> contains (fromChar c) digits) <| toList s) && (isJust <| S.toInt s)
        then Just s
        else Nothing

validateFloat str = case str of
    "" -> Nothing
    s  -> if (and <| map (\c -> contains (fromChar c) <| digits ++ ".") <| toList s) && (isJust <| S.toFloat s)
        then Just s
        else Nothing

validateUnitOfMeasure str = case str of
    "pcs" -> Just str
    "kilograms" -> Just str
    "square_meters" -> Just str
    "centimeters" -> Just str
    _ -> Nothing

