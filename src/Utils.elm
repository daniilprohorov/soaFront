module Utils exposing (..)

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
