module Bingo exposing (main)

import Browser
import Entry
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Random
import Score
import ViewHelpers exposing (..)



-- MODEL


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry.Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }


initialModel : Model
initialModel =
    { name = "Anonymous"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = EnteringName
    }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score.Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none )

        SaveName ->
            if String.isEmpty model.nameInput then
                ( model, Cmd.none )

            else
                ( { model
                    | name = model.nameInput
                    , nameInput = ""
                    , gameState = Playing
                  }
                , Cmd.none
                )

        CancelName ->
            ( { model
                | nameInput = ""
                , gameState = Playing
              }
            , Cmd.none
            )

        SetNameInput value ->
            ( { model | nameInput = value }, Cmd.none )

        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        ShareScore ->
            ( model, Score.postScore NewScore model.name model.entries apiUrlPrefix )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ String.fromInt score.score
                        ++ " was successfully shared!"
            in
            ( { model | alertMessage = Just message }, Cmd.none )

        NewScore (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        NewGame ->
            ( { model
                | gameNumber = model.gameNumber + 1
              }
            , getEntries
            )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = List.sortBy .points randomEntries }, Cmd.none )

        NewEntries (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        Mark id ->
            ( { model | entries = Entry.markEntryWithId model.entries id }, Cmd.none )


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.Timeout ->
            "Request timed out!"

        Http.NetworkError ->
            "Is the server running?"

        Http.BadStatus code ->
            case code of
                401 ->
                    "Unauthorized"

                404 ->
                    "Not Found"

                _ ->
                    "Failed with Status Code: " ++ String.fromInt code

        Http.BadBody message ->
            "Decoding Failed: " ++ message



-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


apiUrlPrefix : String
apiUrlPrefix =
    "http://localhost:3000"


getEntries : Cmd Msg
getEntries =
    Entry.getEntries NewEntries apiUrlPrefix



-- VIEW


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ]
            [ text name ]
        , text (" - Game #" ++ String.fromInt gameNumber)
        ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , alert CloseAlert model.alertMessage
        , viewNameInput model
        , Entry.viewEntryList Mark model.entries
        , Score.viewScore (Entry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game" False
            , primaryButton ShareScore "Share Score" (Score.hasZeroScore model.entries)
            ]
        , viewFooter
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , value model.nameInput
                    , onInput SetNameInput
                    ]
                    []
                , primaryButton SaveName "Save" False
                , primaryButton CancelName "Cancel" False
                ]

        Playing ->
            text ""


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
