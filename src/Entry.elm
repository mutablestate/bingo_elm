module Entry exposing (Entry, getEntries, markEntryWithId, sumMarkedPoints, viewEntryList)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


markEntryWithId : List Entry -> Int -> List Entry
markEntryWithId entries id =
    let
        markEntry e =
            if e.id == id then
                { e | marked = not e.marked }

            else
                e
    in
    List.map markEntry entries


entryDecoder : Decoder Entry
entryDecoder =
    Decode.succeed Entry
        |> required "id" Decode.int
        |> required "phrase" Decode.string
        |> optional "points" Decode.int 100
        |> hardcoded False


getEntries : (Result Http.Error (List Entry) -> msg) -> String -> Cmd msg
getEntries msg apiUrlPrefix =
    Http.get
        { url = apiUrlPrefix ++ "/random-entries"
        , expect = Http.expectJson msg (Decode.list entryDecoder)
        }


viewEntryItem : (Int -> msg) -> Entry -> Html msg
viewEntryItem msg entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (msg entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (String.fromInt entry.points) ]
        ]


viewEntryList : (Int -> msg) -> List Entry -> Html msg
viewEntryList msg entries =
    entries
        |> List.map (viewEntryItem msg)
        |> ul []


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum


allEntriesMarked : List Entry -> Bool
allEntriesMarked entries =
    List.all .marked entries
