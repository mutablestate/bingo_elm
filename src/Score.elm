module Score exposing (Score, hasZeroScore, postScore, viewScore)

import Entry exposing (Entry)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.succeed Score
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "score" Decode.int


encodeScore : String -> List Entry.Entry -> Encode.Value
encodeScore name entries =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "score", Encode.int (Entry.sumMarkedPoints entries) )
        ]


postScore : (Result Http.Error Score -> msg) -> String -> List Entry -> String -> Cmd msg
postScore msg name entries apiUrlPrefix =
    Http.post
        { url = apiUrlPrefix ++ "/scores"
        , body = Http.jsonBody (encodeScore name entries)
        , expect = Http.expectJson msg scoreDecoder
        }


viewScore : Int -> Html msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (String.fromInt sum) ]
        ]


hasZeroScore : List Entry -> Bool
hasZeroScore entries =
    Entry.sumMarkedPoints entries == 0
