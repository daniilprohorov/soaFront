module Utils exposing (..)

import Http exposing (Error(..))
import List exposing (foldl, map)
import String exposing (dropLeft)
import XmlParser exposing (format, Xml, ProcessingInstruction)

nodeToXmlString n = dropLeft 5 <| format <| Xml [ProcessingInstruction "" ""] Nothing n

isOk : Result a b -> Bool
isOk res =
    case res of
        Ok _ -> True
        _    -> False

traverse : (a -> Result String b) -> List a -> Result String (List b)
traverse f l =
    case l of
        []        -> Ok []
        (x :: xs) -> Result.map2 (::) (f x) <| traverse f xs

all : (a -> Bool) -> List a -> Bool
all f list = foldl (&&) True <| map f list


and : List Bool -> Bool
and xs = all (\x -> x) xs


isJust maybe = case maybe of
    Just _ -> True
    _      -> False

formUrlencoded : List ( String, String ) -> String
formUrlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                name
                    ++ "="
                    ++ value
            )
        |> String.join "&"

xmlEncoder : List ( String, String ) -> String
xmlEncoder object =
    object
        |> List.map
            (\( name, value ) ->
                    "<" ++ name ++ ">"
                    ++ value
                    ++ "</" ++ name ++ ">"
            )
        |> String.join "" |> (\s -> "<root>" ++ s ++ "</root>")

errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Timeout ->
            "Unable to reach the server, try again"
        NetworkError ->
            "Unable to reach the server, check your network connection"
        BadStatus 500 ->
            "The server had a problem, try again later"
        BadStatus 400 ->
            "Verify your information and try again"
        BadStatus s -> if s == 404 then "not found" else String.fromInt s
        BadBody errorMessage ->
            errorMessage

