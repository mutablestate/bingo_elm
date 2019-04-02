module ViewHelpers exposing (alert, primaryButton)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


primaryButton : msg -> String -> Bool -> Html msg
primaryButton msg name isDisabled =
    button [ class "primary", onClick msg, disabled isDisabled ] [ text name ]


alert : msg -> Maybe String -> Html msg
alert msg alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick msg ] [ text "X" ]
                , text message
                ]

        Nothing ->
            text ""
