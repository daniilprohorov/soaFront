module Validation.Fields exposing (..)
import List exposing (map)
import String exposing (contains, fromChar, toList)
import Utils exposing (and)

letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

validateName str = case str of
    "" -> Err "Name field is blank"
    s  -> if and <| map (\c -> contains (fromChar c) letters) <| toList str
        then Ok str
        else Err "not valid string"

