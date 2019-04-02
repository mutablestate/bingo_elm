module DecodingPlayground exposing (decoder, decodingResult, json, main, view)

import Html exposing (..)
import Json.Decode exposing (..)


json =
    """
    {
      "id": 1,
      "phrase": "Future-Proof",
      "points": 100
    }
    """


decoder =
    field "id" int


decodingResult =
    decodeString decoder json


view result =
    case result of
        Ok value ->
            text (String.fromInt value)

        Err err ->
            text (String.fromInt err)


main =
    view decodingResult
