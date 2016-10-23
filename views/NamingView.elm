module NamingView exposing (..)

import Html
import Html.Events exposing (..)
import Html.Attributes exposing (autofocus, type')


namingView name input submit =
    Html.div []
        [ Html.h2 [] [Html.text "Connect to a room."]
        , Html.span [] [ Html.text "Plese select a room name: " ]
        , Html.input [ onInput input, autofocus True ] []
        , Html.button [ onClick submit ] [ Html.text "Connect" ]
        ]
